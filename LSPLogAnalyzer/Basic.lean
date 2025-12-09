import Lean
import LSPLogAnalyzer.Definitions
import LSPLogAnalyzer.MyParser

namespace LSPLogAnalyzer

open Lean.Json
open Lean.FromJson
open Lean.JsonRpc
open Lean.Lsp

open Std.Time

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

def getLine : TrackerM Nat := do
  return (← get).line

def collectLogEntries (path : System.FilePath) : IO (Array LogEntry) := do
  let log ← IO.FS.readFile path
  let log := log.trimRight
  let entries := log.splitOn "\n" |>.toArray
  let entries := entries.map parse
  let entries ← IO.ofExcept <| entries.mapM id
  IO.ofExcept <| entries.mapM fromJson?

def modifyFileState (uri : Uri) (fs : FileState) : TrackerM Unit := do
  modify fun s => { s with files := s.files.insert uri fs }

end Utils

section TrackerActions

def onError (e : String)
  -- (entry : LogEntry)
  : TrackerM Unit := do
  modify fun st => { st with errors := st.errors.push ⟨e⟩ }

def onDidOpen (time : ZonedDateTime) (notif : Notification Lean.Json) : TrackerM Unit := do
  let .ok (params : LeanDidOpenTextDocumentParams) := fromJson? notif.param
    | onError "Unable to parse didOpen parameters {entry.param}"
  let doc := params.textDocument
  let fs ← ensureFile doc.uri
  let deflikes ← runCollectDefLikes doc.uri doc.text
  let fs := { fs with
    snapshots := fs.snapshots.push ⟨time, doc, #[], #[], deflikes⟩
    changes := #[] }
  modifyFileState doc.uri fs

def onDidChange (time : ZonedDateTime) (notif : Notification Lean.Json) : TrackerM Unit := do
  let .ok (params : DidChangeTextDocumentParams) := fromJson? notif.param
    | onError "Unable to parse didChange parameters {entry.param}"
  let doc := params.textDocument
  let changes := params.contentChanges
  let fs ← ensureFile doc.uri
  let .some v := doc.version? | onError s!"Missing version in didChange event on line {← getLine}"
  let fs := { fs with changes := fs.changes.push ⟨time, v, changes⟩}
  modifyFileState doc.uri fs

def updateSnapshot (s : Snapshot) (changes : Array ChangeEvent): Snapshot :=
  match changes.back? with
  | none => s
  | some { time := t, version := v, .. } =>
    let changes := changes.flatMap (·.changes)
    let newText := Lean.Server.foldDocumentChanges changes s.doc.text.toFileMap
    let newdoc := { s.doc with
      text := newText.source  -- avoid converting back and forth?
      version := v }
    {s with doc := newdoc, time := t, goals := #[] }

def updateDiagnostics (uri : Uri) (fs : FileState) (diags : Array Diagnostic) : TrackerM Unit := do
  let .some s := fs.snapshots.back? | onError s!"No existing snapshot for {uri} in updateDiagnostics"
  if s.diags == diags then return
  let snapshots := match fs.changes with
  | #[] =>
    let s := { s with diags := diags }
    fs.snapshots.pop.push s
  | changes =>
    let s := { updateSnapshot s changes with diags := diags }
    fs.snapshots.push s
  modifyFileState uri { fs with snapshots := snapshots, changes := #[] }

def onPublishDiagnostics (notif : Notification Lean.Json) : TrackerM Unit := do
  let .ok (params : PublishDiagnosticsParams) := fromJson? notif.param
    | onError s!"Unable to parse publishDiagnostics parameters {notif.param}"
  let fs ← ensureFile params.uri
  updateDiagnostics params.uri fs params.diagnostics

def onRpcResponse (request : Request Lean.Json) (response : Response Lean.Json) : TrackerM Unit := do
  return

def onGetInteractiveGoalsResponse (response : Response Lean.Json) : TrackerM Unit := do
  return  -- FIXME

end TrackerActions


section Processing

def processNotification
    (time : ZonedDateTime)
    (dir : MessageDirection)
    (notif : Notification Lean.Json) : TrackerM Unit := do
  match dir, notif.method with
  | .clientToServer, "textDocument/didOpen" => onDidOpen time notif
  | .clientToServer, "textDocument/didChange" => onDidChange time notif
  | .serverToClient, "textDocument/publishDiagnostics" => onPublishDiagnostics notif
  | _, _ => onError s!"Ignored notification {notif.method} on line {← getLine}"

def processRequest (request : Request Lean.Json) : TrackerM Unit := do
  modify fun ts => { ts with requests := ts.requests.insert request.id request }

def processResponse
    (time : ZonedDateTime)
    (dir : MessageDirection)
    (response : Response Lean.Json) : TrackerM Unit := do
  let ts ← get
  let .some req := ts.requests.get? response.id | onError s!"Ignored response {response.id} on line {← getLine} (no such pending request)"
  match dir, req.method with
  | .clientToServer, "$/lean/rpc/call" => onRpcResponse req response
  | _, _ => onError s!"Ignored response {response.id} ({req.method}) on line {← getLine}"
  set { ts with requests := ts.requests.erase response.id }

def processLogEntry (entry : LogEntry) : TrackerM Unit := do
  match entry.kind with
  | .notification  =>
      let .some notif := Notification.ofMessage? entry.msg
        | onError s!"Unable to parse notification {entry} on line {← getLine}"
      processNotification entry.time entry.direction notif
  | .request =>
      let .some request := Request.ofMessage? entry.msg
        | onError s!"Unable to parse request {entry} on line {← getLine}"
      processRequest request
  | .response =>
      let .some resp := Response.ofMessage? entry.msg
        | onError s!"Unable to parse response {entry}"
      processResponse entry.time entry.direction resp
  | _ => onError s!"Ignored log entry ({entry.direction}) on line {← getLine}: {messageSummary entry.msg}"
  modify fun ts => { ts with line := ts.line + 1 }

open System in
def processLogFile (path : FilePath) (watched? : Option (List Uri) := none): TrackerM Unit := do
  let entries ← collectLogEntries path
  -- let p := match watched? with
  -- | some uris => fun (s : String) => uris.any (s.toSlice.contains ·)
  -- | none => fun _ => true
  entries.forM processLogEntry

end Processing

end LSPLogAnalyzer
