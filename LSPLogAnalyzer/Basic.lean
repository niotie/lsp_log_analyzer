import Lean.Data.JsonRpc
import Lean.Data.Lsp
import Lean.Data.RBMap
import Lean.Server.Logging
import Lean.Server.ProtocolOverview

namespace LSPLogAnalyzer

open Lean.Json Lean.ToJson Lean.FromJson Lean.JsonRpc Lean.Lsp
open Std.Time

abbrev Uri := String

local instance : Lean.ToJson ZonedDateTime where
  toJson dt := dt.toISO8601String

local instance : Lean.FromJson ZonedDateTime where
  fromJson?
    | .str s => ZonedDateTime.fromISO8601String s
    | _ => throw "Expected string when converting JSON to ZonedDateTime"


section Structures

/-- LSP log entry (new format). -/
structure LogEntry where
  time : ZonedDateTime
  direction : MessageDirection
  kind : MessageKind
  msg : Message
  deriving Lean.FromJson, Lean.ToJson

instance : ToString LogEntry where
  toString le := pretty $ toJson le

instance : ToString LogEntry where
  toString le := pretty $ toJson le

instance : Coe LogEntry Message := ⟨LogEntry.msg⟩


/-- File change event. -/
structure ChangeEvent where
  time : ZonedDateTime
  version? : Option Nat
  changes : Array TextDocumentContentChangeEvent

local instance : ToString Range where
  toString | ⟨⟨sl, sc⟩, ⟨el, ec⟩⟩ => s!"[{sl}:{sc}, {el}:{ec}]"

local instance : ToString TextDocumentContentChangeEvent where
  toString ev := match ev with
  | .fullChange _ => "full text"
  | .rangeChange range text => s!"\"{text}\" at {range}"

instance : ToString ChangeEvent where
  toString ev :=
    let ranges := ev.changes.map toString
    let sranges := ranges.toList.map toString
    let vs :=
      if h : ev.version?.isSome then
        let v := ev.version?.get h
        s!" - version {v}"
      else ""
    s!"[{ev.time}{vs}] " ++ ", ".intercalate sranges


/-- File snapshot. -/
structure Snapshot where
  time : ZonedDateTime
  doc : TextDocumentItem

instance : ToString Snapshot where
  toString snap :=
    let v := snap.doc.version
    s!"[{snap.time} - version {v}]\n{snap.doc.text}\n[end version {v}]"


/-- Per-file replay state. -/
structure FileState where
  snapshots : Array Snapshot := #[]
  currentSnap : Option Snapshot := none
  changes : Array ChangeEvent := #[]
  diagnostics : Array Diagnostic := #[]
deriving Inhabited

instance : ToString FileState where
  toString fs :=
    let ssnaps := String.intercalate "\n" $ fs.snapshots.toList.map toString
    let schanges := String.intercalate "\n" $ fs.changes.toList.map toString
    "Recorded snapshots:\n" ++ ssnaps
    ++ "\n\nPending changes:\n" ++ schanges

/-- Log entry tracking error. -/
structure TrackingError where
  message : String
  -- entry : LogEntry

instance : ToString TrackingError where
  toString te := te.message


/-- Global tracker. -/
structure Tracker where
  files : Std.TreeMap Uri FileState Ord.compare := {}
  errors : Array TrackingError := #[]
deriving Inhabited

instance : ToString Tracker where
  toString tracker :=
    let aux | (uri, fs) => s!"State for {uri}:\n\n" ++ toString fs
    let fss := String.intercalate "\n\n" $ tracker.files.toList.map aux
    let errors := String.intercalate "\n" $ tracker.errors.toList.map toString
    fss ++ "\n\nErrors:\n" ++ errors

/-- Tracker monad. -/
abbrev TrackerM := StateT Tracker IO

end Structures


section Utils

def fileStream (filename : System.FilePath) : IO IO.FS.Stream := do
  let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
  pure (IO.FS.Stream.ofHandle handle)

def parseLogLine (line : String) : Except String LogEntry := do
  let j ← parse line
  Lean.fromJson? j

private def ensureFile (uri : String) : TrackerM FileState := do
  let st ← get
  match st.files.get? uri with
  | some fs => return fs
  | none =>
    let fs : FileState := {}
    set { st with files := st.files.insert uri fs }
    return fs

def messageSummary : Message → String
  | .request id method@"$/lean/rpc/call" (some params) =>
      let rpcmethod := params.toJson.getObjValAs? String "method" |>.toOption
      s!"request {id} - {method} {rpcmethod.getD "unknown method"}"
  | .request id method _params? => s!"request {id} - {method}"
  | .notification method _params? => s!"notification - {method}"
  | .response id _result => s!"response {id}"
  | .responseError id _errCode _msg _params? => s!"response error {id}"

def logEntrySummary (e : LogEntry) : String := messageSummary e.msg

partial def collectMessages (stream : IO.FS.Stream) (filter : Message → Bool) : IO (List Message) := do
  try
    let msg ← IO.FS.Stream.readLspMessage stream
    let tail ← collectMessages stream filter
    if filter msg then
      pure (msg :: tail)
    else
      pure tail
  catch e =>
    if e.toString.endsWith "Stream was closed" then
      pure []
    else
      let stderr ← IO.getStderr
      stderr.putStrLn s!"{e}"
      collectMessages stream filter

partial def collectLogEntries (stream : IO.FS.Stream) (filter : Message → Bool) : IO (List LogEntry) := do
  match (← stream.getLine) with
  | "" => pure []
  | line =>
      let j ← IO.ofExcept $ parse line
      let entry : Except String LogEntry := Lean.fromJson? j
      match entry with
      | .error e =>
          (← IO.getStderr).putStrLn e
          tail
      | .ok entry =>
          if filter entry then
            return entry :: (← tail)
          else
            tail
      where tail := collectLogEntries stream filter

def updateSnapshot (snap : Snapshot) : Option Snapshot :=
  snap  -- FIXME

def modifyFileState (uri : Uri) (fs : FileState) : TrackerM Unit := do
  modify fun s => { s with files := s.files.insert uri fs }

end Utils


section Unused

-- partial def processLines
--     (stream : IO.FS.Stream)
--     (processor : String → Except String α)
--     : IO (List α) := do
--   match (← stream.getLine) with
--   | "" => pure []
--   | line =>
--       let head := processor line
--       let tail ← processLines stream processor
--       match head with
--       | .error e =>
--           (← IO.getStderr).putStrLn e
--           return tail
--       | .ok entry =>
--           return entry :: tail

-- def getUri (msg : Message) : Option String :=
--     match msg with
--     | .request _ _ (some params)
--     | .notification _ (some params) =>
--         match do
--           let params := params.toJson
--           let textDoc ← params.getObjVal? "textDocument"
--           let uri ← textDoc.getObjVal? "uri"
--           uri.getStr?
--         with
--         | .ok s => some s
--         | _ => none
--     | _ => none

-- def exampleMessageFilter (msg : Message) : Bool :=
--   match msg with
--   | .request _ "initialize" .. => true
--   | .notification "textDocument/didOpen" ..
--   | .notification "textDocument/didChange" .. =>
--     match getUri msg with
--     | none => false
--     | some s => s.endsWith "Example.lean"
--   | _ => false

end Unused


section TrackerActions

def onError (e : String)
  -- (entry : LogEntry)
  : TrackerM Unit := do
  let st ← get
  set { st with errors := st.errors.push ⟨e⟩ }

def onDidOpen (time : ZonedDateTime) (params? : Option Structured) : TrackerM Unit := do
  match (fromJson? (toJson params?) : Except String LeanDidOpenTextDocumentParams) with
  | .error e => onError e; return
  | .ok params =>
    let doc := params.textDocument
    let fs ← ensureFile doc.uri
    let fs := { fs with
      snapshots := fs.snapshots.push ⟨time, doc⟩
      changes := #[] }
    modifyFileState doc.uri fs

def onDidChange (time : ZonedDateTime) (params? : Option Structured) : TrackerM Unit := do
  match (fromJson? (toJson params?) : Except String DidChangeTextDocumentParams) with
  | .error e => onError e; return
  | .ok params =>
    let doc := params.textDocument
    let changes := params.contentChanges
    let v? := doc.version?
    let fs ← ensureFile doc.uri
    let fs := { fs with
      changes := fs.changes.push ⟨time, v?, changes⟩}
    modifyFileState doc.uri fs

end TrackerActions


section Processing

-- def processParams {paramType : Type} [self : Lean.FromJson paramType] (params : Structured) : Option paramType := do
--   let j := toJson params
--   match fromJson? j with
--   | .ok params => return params
--   | .error e => onError e; return none

def processNotification (time : ZonedDateTime) (method : String) (params? : Option Structured) : TrackerM Unit := do
  match method with
  | "textDocument/didOpen" => onDidOpen time params?
  | "textDocument/didChange" => onDidChange time params?
  | _ => onError s!"Ignored log entry: notification {method}"
  return

def processLogEntry (entry : LogEntry) : TrackerM Unit := do
  match entry.direction with
  | .clientToServer =>
    match entry.msg with
    | .notification method params? => processNotification entry.time method params?
    -- | .request id method params? => onError s!"Ignored log entry: {entry}"
    | _ => onError s!"Ignored log entry (client to server): {messageSummary entry.msg}"
  | .serverToClient =>
    match entry.msg with
    | _ => onError s!"Ignored log entry (server to client): {messageSummary entry.msg}"

end Processing
