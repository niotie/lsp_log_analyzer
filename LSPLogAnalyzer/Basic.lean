import Lean
import LSPLogAnalyzer.Definitions
import LSPLogAnalyzer.Diagnostics
import LSPLogAnalyzer.Utils
import LSPLogAnalyzer.MyParser

open Lean.Json
open Lean.FromJson
open Lean.JsonRpc
open Lean.Lsp

open Std.Time

namespace LSPLogAnalyzer

def onDidOpen (time : ZonedDateTime) (notif : Notification Lean.Json) : TrackerM Unit := do
  let .ok (params : LeanDidOpenTextDocumentParams) := fromJson? notif.param
    | onError s!"Unable to parse didOpen parameters {notif.param}"
  let doc := params.textDocument
  let fs ← ensureFile doc.uri
  let defArray ← runCollectDefLikes fs.baseEnv doc
  let defMap := .ofList <| defArray.toList.map fun d => (d.name.toString, #[d])
  let snapshots := fs.snapshots.push ⟨time, doc, #[], #[]⟩
  let fs := { fs with snapshots, defMap, defArray, changes := #[] }
  modifyFileState doc.uri fs

def onDidChange (time : ZonedDateTime) (notif : Notification Lean.Json) : TrackerM Unit := do
  let .ok (params : DidChangeTextDocumentParams) := fromJson? notif.param
    | onError s!"Unable to parse textDocument/didChange parameters {notif.param}"
  let doc := params.textDocument
  let changes := params.contentChanges
  let fs ← ensureFile doc.uri
  let .some v := doc.version?
    | onError s!"Missing version in didChange event on line {← getLine}"
  let fs := { fs with changes := fs.changes.push ⟨time, v, changes⟩}
  modifyFileState doc.uri fs

def onPublishDiagnostics (notif : Notification Lean.Json) : TrackerM Unit := do
  let .ok (params : PublishDiagnosticsParams) := fromJson? notif.param
    | onError s!"Unable to parse publishDiagnostics parameters {notif.param}"
  updateDiagnostics params.uri params.diagnostics

def onRpcResponse (_request : Request Lean.Json) (_response : Response Lean.Json) : TrackerM Unit := do
  return

def onGetInteractiveGoalsResponse (_response : Response Lean.Json) : TrackerM Unit := do
  return  -- FIXME

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
    (_time : ZonedDateTime)
    (dir : MessageDirection)
    (response : Response Lean.Json) : TrackerM Unit := do
  let ts ← get
  let .some req := ts.requests.get? response.id
    | onError s!"Ignored response {response.id} on line {← getLine} (no such pending request)"
  match dir, req.method with
  -- | .clientToServer, "$/lean/rpc/call" => onRpcResponse req response
  | _, _ => onError s!"Ignored response {response.id} ({req.method}) on line {← getLine}"
  set { ts with requests := ts.requests.erase response.id }

def processResponseError
    (_time : ZonedDateTime)
    (_dir : MessageDirection)
    (response : ResponseError Lean.Json) : TrackerM Unit := do
  let ts ← get
  let .some _ := ts.requests.get? response.id
    | onError s!"Ignored response error {response.id} on line {← getLine} (no such pending request)"
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
        | onError s!"Unable to parse response {entry} on line {← getLine}"
      processResponse entry.time entry.direction resp
  | .responseError =>
      let .some respErr := ResponseError.ofMessage? entry.msg
        | onError s!"Unable to parse response error {entry} on line {← getLine}"
      processResponseError entry.time entry.direction respErr
  modify fun ts => { ts with line := ts.line + 1 }

def processLogFile (path : System.FilePath) : TrackerM Unit := do
  let entries ← collectLogEntries path
  entries.forM processLogEntry

end LSPLogAnalyzer
