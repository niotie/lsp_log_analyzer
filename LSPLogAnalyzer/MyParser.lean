import Lean
import LSPLogAnalyzer.Definitions

open Lean
open Parser
open Parser.Command
open Elab
open Elab.Command
open System
open Lsp

open Std

namespace LSPLogAnalyzer

def collectDefLikes (doc : TextDocumentItem) : CommandElabM (Array Definition) := do
  let env ← getEnv
  let inCtx := mkInputContext doc.text doc.uri
  let pmCtx := { env, options := {} }
  let (headerStx, mps, msgl) ← parseHeader inCtx
  -- TODO : elab the header? (not forgetting to update env)

  let mut mps := mps
  let mut msgl := msgl
  let mut res := #[]
  let mut cmdStx := .missing

  repeat
    (cmdStx, mps, msgl) := parseCommand inCtx pmCtx mps msgl
    -- let _ ← elabCommand cmdStx  -- TODO : necessary ?
    if cmdStx.getNumArgs > 1 && isDefLike cmdStx[1] then
      let defview ← mkDefView {} cmdStx[1]
      let defn := {
        name := defview.getId
        version := doc.version
        kind := defview.kind
        range? := some $ defview.getLspRange inCtx
        defview? := some defview
      }
      res := res.push defn
  until isTerminalCommand cmdStx

  return res

-- def runCollectDefLikes (fname : FilePath) (contents : String)
--     : IO (Array Definition) := do
--   let action := collectDefLikes fname contents
--   let ctx := {
--     fileName := fname.toString,
--     fileMap := FileMap.ofString contents,
--     snap? := none,
--     cancelTk? := none }
--   let env ← mkEmptyEnvironment
--   let s := { env := env, maxRecDepth := 100000 }
--   action.run ctx |>.run' s |>.toIO
--     fun _ => IO.Error.mkOtherError 0 "dunno"


def runCollectDefLikes (env : Environment) (doc : TextDocumentItem)
    : IO (Array Definition) := do
  let ctx : Context := {
    fileName := doc.uri,
    fileMap := FileMap.ofString doc.text,
    snap? := none,
    cancelTk? := none }
  let s : State := { env := env, maxRecDepth := 100 }
  let action := collectDefLikes doc
  action.run ctx |>.run' s |>.toIO fun e =>
    IO.Error.mkOtherError 0 s!"error collecting definitions : {e.getRef}"

end LSPLogAnalyzer
