import Lean
import LSPLogAnalyzer.Definitions

open Lean.Json
open Lean.FromJson
open Lean.JsonRpc
open Lean.Lsp

namespace LSPLogAnalyzer

open IO.FS in
def fileStream (filename : System.FilePath) : IO Stream := do
  return .ofHandle (← Handle.mk filename Mode.read)

def parseLogLine (line : String) : Except String LogEntry := do
  Lean.fromJson? (← parse line)

def ensureFile (uri : String) : TrackerM FileState := do
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

def locatePos (defs : Array Definition) (pos : Lean.Lsp.Position)
    (lo : Nat := 0) (hi : Nat := defs.size) : Option (Nat × Definition) :=
  if hi ≤ lo then none
  else
    let m := (lo + hi) / 2
    defs[m]? >>= fun d =>
      if pos ∈ d.range then
        (m, d)
      else if pos < d.range.start then
        locatePos defs pos lo m
      else
        locatePos defs pos (m+1) hi
  termination_by hi - lo

def relativePos (origin pos : Position) : Position :=
  if origin.line == pos.line then
    ⟨0, pos.character - origin.character⟩
  else
    ⟨pos.line - origin.line, pos.character⟩

def relativeRange (enclosing inner : Range) : Range :=
  ⟨relativePos enclosing.start inner.start, relativePos enclosing.start inner.end⟩

def updateDefDiag (defn : Definition) (diag : Diagnostic) : Definition :=
  let diag := { diag with range := relativeRange defn.range diag.range }
  { defn with diags := defn.diags.push diag }

end LSPLogAnalyzer
