import Lean
import LSPLogAnalyzer.Definitions
import LSPLogAnalyzer.Utils
import LSPLogAnalyzer.MyParser

open Lean.Json
open Lean.FromJson
open Lean.JsonRpc
open Lean.Lsp

open Std.Time

namespace LSPLogAnalyzer

section TrackerActions

def onError (e : String)
  -- (entry : LogEntry)
  : TrackerM Unit := do
  modify fun st => { st with errors := st.errors.push ⟨e⟩ }

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

def maybeUpdateDefs
    (defMap : Std.HashMap String (Array Definition))
    (defArray : Array Definition)
    : Std.HashMap String (Array Definition) :=
  defArray.foldl go defMap
  where
    go defMap newDef :=
      defMap.alter newDef.name.toString fun defs? =>
        match defs? >>= Array.back? with
        | none => some #[newDef]
        | some oldDef =>
            -- TODO : fixme, never pushes anything
            if newDef.localDiags.all fun d => oldDef.localDiags.any fun d' => d == d' then
              defs?
            else
              let newStx := newDef.defview?.map (·.ref)
              let oldStx := oldDef.defview?.map (·.ref)
              if newStx == oldStx then
                defs? >>= (·.pop.push newDef)
              else
                defs? >>= (Array.push · newDef)

def recordLocalDiags
    (defs : Array Definition)
    (diags : Array Diagnostic)
    : (Array Definition × Array Diagnostic) :=
  diags.foldl go (defs, #[])
  where
    go acc diag :=
      let (res, remainingDiags) := acc
      let tmp : Option (Nat × LocalDiagnostic) := do
        let (i, defn) ← locatePos defs diag.range.start
        let dr ← defn.range?
        let localRange := relativeRange dr diag.range
        let localDiag := { diag, localRange }
        return (i, localDiag)
      match tmp with
      | none => (res, remainingDiags.push diag)
      | some (i, ld) => (res.modify i fun d =>
        { d with localDiags := d.localDiags.push ld }, remainingDiags)

def normalizeText (text : String) : String :=
  let lines := text.splitOn "\n"
    |>.map String.trimRight
    |>.filter (not ·.isEmpty)
  "\n".intercalate lines

def updateDiagnostics (uri : Uri) (diags : Array Diagnostic) : TrackerM Unit := do
  let fs ← ensureFile uri
  let .some s := fs.snapshots.back?
    | onError s!"No existing snapshot for {uri} in updateDiagnostics"
  -- if s.diags == diags then return
  let (doc, defArray, time, docHasChanged?) ← match fs.changes.back? with
  | none => pure (s.doc, fs.defArray, s.time, false)
  | some { time, version, .. } =>
    let changes := fs.changes.flatMap (·.changes)
    let newText := Lean.Server.foldDocumentChanges changes s.doc.text.toFileMap
    if normalizeText newText.source == normalizeText s.doc.text then
      pure (s.doc, fs.defArray, s.time, false)
    else
      -- TODO : avoid converting back and forth?
      let newdoc := { s.doc with version, text := newText.source }
      let defArray ← runCollectDefLikes fs.baseEnv newdoc
      pure (newdoc, defArray, time, true)
  let (defArray, globalDiags) := recordLocalDiags defArray diags
  let s := {s with doc := doc, time, globalDiags }
  let fs := if docHasChanged? then
    { fs with
      snapshots := fs.snapshots.push s
      defArray
      defMap := maybeUpdateDefs fs.defMap defArray
      changes := #[] }
  else
    { fs with
      snapshots := fs.snapshots.pop.push s
      defMap := maybeUpdateDefs fs.defMap defArray }
  modifyFileState uri fs

def onPublishDiagnostics (notif : Notification Lean.Json) : TrackerM Unit := do
  let .ok (params : PublishDiagnosticsParams) := fromJson? notif.param
    | onError s!"Unable to parse publishDiagnostics parameters {notif.param}"
  updateDiagnostics params.uri params.diagnostics

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
  let .some req := ts.requests.get? response.id
    | onError s!"Ignored response {response.id} on line {← getLine} (no such pending request)"
  match dir, req.method with
  -- | .clientToServer, "$/lean/rpc/call" => onRpcResponse req response
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
