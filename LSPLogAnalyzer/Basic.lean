import Lean

namespace LSPLogAnalyzer

open Lean.Json
open Lean.ToJson
open Lean.FromJson
open Lean.JsonRpc
open Lean.Lsp
open Std.Time
open Lean.Server.Test.Runner.Client

abbrev Uri := String

section Structures

instance : Lean.ToJson ZonedDateTime where
  toJson dt := dt.toISO8601String

instance : Lean.FromJson ZonedDateTime where
  fromJson?
    | .str s => ZonedDateTime.fromISO8601String s
    | _ => throw "Expected string when converting JSON to ZonedDateTime"

/-- LSP log entry (new format). -/
structure LogEntry where
  time : ZonedDateTime
  direction : MessageDirection
  kind : MessageKind
  msg : Message
  deriving Lean.FromJson, Lean.ToJson

instance : ToString LogEntry where
  toString le := pretty $ toJson le

instance : ToString MessageDirection where
  toString
  | .serverToClient => "server→client"
  | .clientToServer => "client→server"

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
    let vs :=
      match ev.version? with
      | some v => s!" - version {v}"
      | none => ""
    s!"[{ev.time}{vs}] " ++ ", ".intercalate ranges.toList


/-- File snapshot. -/
structure Snapshot where
  time : ZonedDateTime
  doc : TextDocumentItem
  goals : Array InteractiveGoal  -- not used yet
  diags : Array Diagnostic       -- not used yet

instance : ToString Snapshot where
  toString snap :=
    let v := snap.doc.version
    s!"[{snap.time} - version {v}]\n{snap.doc.text}\n[end version {v}]"


/-- Per-file replay state. -/
structure FileState where
  snapshots : Array Snapshot := #[]
  changes : Array ChangeEvent := #[]
  currentSnap : Option Snapshot := none   -- not used yet
  currentDiags : Array Diagnostic := #[]  -- not used yet
  requests : Std.TreeMap RequestID Lean.Name := {}
deriving Inhabited

instance : ToString FileState where
  toString fs :=
    let ssnaps := String.intercalate "\n" $ fs.snapshots.toList.map toString
    let schanges := String.intercalate "\n" $ fs.changes.toList.map toString
    let requests := "\n".intercalate $ fs.requests.toList.map toString
    s!"Recorded snapshots:\n{ssnaps}\n\n\
       Pending changes:\n{schanges}\n\n\
       Pending requests:\n{requests}"

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
  line : Nat := 1
deriving Inhabited

instance : ToString Tracker where
  toString tracker :=
    let fss := "\n\n".intercalate $ tracker.files.toList.map
      fun (uri, fs) => s!"State for {uri}:\n\n{fs}"
    let errors := "\n".intercalate $ tracker.errors.toList.map toString
    fss ++ "\n\nErrors:\n" ++ errors

/-- Tracker monad. -/
abbrev TrackerM := StateT Tracker IO

end Structures


section Utils

open IO.FS in
def fileStream (filename : System.FilePath) : IO Stream := do
  return .ofHandle (← Handle.mk filename Mode.read)

def parseLogLine (line : String) : Except String LogEntry := do
  Lean.fromJson? (← parse line)

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

open IO FS in
partial def collectMessages (stream : Stream) (filter : Message → Bool) : IO (List Message) := do
  try
    let msg ← stream.readLspMessage
    let tail ← collectMessages stream filter
    if filter msg then
      pure (msg :: tail)
    else
      pure tail
  catch e =>
    if e.toString.endsWith "Stream was closed" then
      pure []
    else
      let stderr ← getStderr
      stderr.putStrLn s!"{e}"
      collectMessages stream filter

partial def collectLogEntries (stream : IO.FS.Stream) (filter : Message → Bool) : IO (List LogEntry) := do
  match ← stream.getLine with
  | "" => pure []
  | line =>
      let j ← .ofExcept $ parse line
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

def collectLogEntries' (path : System.FilePath) : IO (Array LogEntry) := do
  let log ← IO.FS.readFile path
  let log := log.trimRight
  let entries := log.splitOn "\n" |>.toArray
  let entries := entries.map parse
  let entries ← IO.ofExcept <| entries.mapM id
  IO.ofExcept <| entries.mapM fromJson?

def updateSnapshot (snap : Snapshot) : Option Snapshot :=
  snap  -- FIXME

def modifyFileState (uri : Uri) (fs : FileState) : TrackerM Unit := do
  modify fun s => { s with files := s.files.insert uri fs }

end Utils

section TrackerActions

def onError (e : String)
  -- (entry : LogEntry)
  : TrackerM Unit := do
  modify fun st => { st with errors := st.errors.push ⟨e⟩ }

-- def onDidOpen (time : ZonedDateTime) (params? : Option Structured) : TrackerM Unit := do
--   match (fromJson? (toJson params?) : Except String LeanDidOpenTextDocumentParams) with
--   | .error e => onError e; return
--   | .ok params =>
--     let doc := params.textDocument
--     let fs ← ensureFile doc.uri
--     let fs := { fs with
--       snapshots := fs.snapshots.push ⟨time, doc, #[], #[]⟩
--       changes := #[] }
--     modifyFileState doc.uri fs

def onDidOpen' (time : ZonedDateTime) (notif : Notification Lean.Json) : TrackerM Unit := do
  let .ok (params : LeanDidOpenTextDocumentParams) := fromJson? notif.param
    | onError "Unable to parse didOpen parameters {entry.param}"
  let doc := params.textDocument
  let fs ← ensureFile doc.uri
  let fs := { fs with
    snapshots := fs.snapshots.push ⟨time, doc, #[], #[]⟩
    changes := #[] }
  modifyFileState doc.uri fs

-- def onDidChange (time : ZonedDateTime) (params? : Option Structured) : TrackerM Unit := do
--   match (fromJson? (toJson params?) : Except String DidChangeTextDocumentParams) with
--   | .error e => onError e; return
--   | .ok params =>
--     let doc := params.textDocument
--     let changes := params.contentChanges
--     let v? := doc.version?
--     let fs ← ensureFile doc.uri
--     let fs := { fs with
--       changes := fs.changes.push ⟨time, v?, changes⟩}
--     modifyFileState doc.uri fs

def onDidChange' (time : ZonedDateTime) (notif : Notification Lean.Json) : TrackerM Unit := do
  let .ok (params : DidChangeTextDocumentParams) := fromJson? notif.param
    | onError "Unable to parse didChange parameters {entry.param}"
  let doc := params.textDocument
  let changes := params.contentChanges
  let fs ← ensureFile doc.uri
  let fs := { fs with
    changes := fs.changes.push ⟨time, doc.version?, changes⟩}
  modifyFileState doc.uri fs

def onRpcRequest (request : Request Lean.Json) : TrackerM Unit := do
  -- TODO : filter out uninteresting requests?
  let .ok (params : RpcCallParams) := fromJson? request.param
    | onError "Unable to parse didChange parameters {entry.param}"
  let doc := params.textDocument
  let fs ← ensureFile doc.uri
  let fs := { fs with
    requests := fs.requests.insert request.id params.method }
  modifyFileState doc.uri fs

def onGetInteractiveGoalsResponse (response : Response Lean.Json) : TrackerM Unit := do
  sorry

end TrackerActions


section Processing

-- def processParams {paramType : Type} [self : Lean.FromJson paramType] (params : Structured) : Option paramType := do
--   let j := toJson params
--   match fromJson? j with
--   | .ok params => return params
--   | .error e => onError e; return none

-- def processNotification
--     (time : ZonedDateTime)
--     (_dir : MessageDirection)
--     (method : String)
--     (params? : Option Structured) : TrackerM Unit := do
--   match method with
--   | "textDocument/didOpen" => onDidOpen time params?
--   | "textDocument/didChange" => onDidChange time params?
--   | _ => onError s!"Ignored log entry: notification {method}"

def processNotification'
    (time : ZonedDateTime)
    (dir : MessageDirection)
    (notif : Notification Lean.Json) : TrackerM Unit := do
  match dir, notif.method with
  | .clientToServer, "textDocument/didOpen" => onDidOpen' time notif
  | .clientToServer, "textDocument/didChange" => onDidChange' time notif
  | _, _ => onError s!"Ignored log entry: notification {notif.method}"

def processRequest
    (dir : MessageDirection)
    (request : Request Lean.Json) : TrackerM Unit := do
  match dir, request.method with
  | .clientToServer, "$/lean/rpc/call" => onRpcRequest request
  | _, _ => onError s!"Ignored log entry: request {request.method}"

def processResponse
    (time : ZonedDateTime)
    (dir : MessageDirection)
    (response : Response Lean.Json) : TrackerM Unit := do
  sorry

def processLogEntry (entry : LogEntry) : TrackerM Unit := do
  match entry.kind with
  | .notification .. =>
      let .some notif := Notification.ofMessage? entry.msg
        | onError "Unable to parse notification {entry.msg}"
      processNotification' entry.time entry.direction notif
  -- | .request id method params? => onError s!"Ignored log entry: {entry}"
  | .request .. =>
      let .some request := Request.ofMessage? entry.msg
        | onError "Unable to parse request {entry.msg}"
      processRequest entry.direction request
  | .response .. =>
      let .some resp := Response.ofMessage? entry.msg
        | onError "Unable to parse response {entry.msg}"
      processResponse entry.time entry.direction resp
  | _ => onError s!"Ignored log entry ({entry.direction}): {messageSummary entry.msg}"

end Processing

end LSPLogAnalyzer
