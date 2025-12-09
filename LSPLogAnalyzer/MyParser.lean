import Lean
import LSPLogAnalyzer.Definitions

open Lean
open Parser
open Parser.Command
open Elab
open Elab.Command
open System

open Std


namespace LSPLogAnalyzer

def collectDefLikes (fname : FilePath) (contents : String)
    : CommandElabM (Array Definition) := do
  let env ← getEnv
  let inCtx := mkInputContext contents fname.toString
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
      let name := defview.getId
      let range := defview.getLspRange inCtx
      res := res.push { name, defview, range }
  until isTerminalCommand cmdStx

  return res

def runCollectDefLikes (fname : FilePath) (contents : String)
    : IO (Array Definition) := do
  let action := collectDefLikes fname contents
  let ctx := {
    fileName := fname.toString,
    fileMap := FileMap.ofString contents,
    snap? := none,
    cancelTk? := none }
  let env ← mkEmptyEnvironment
  let s := { env := env, maxRecDepth := 10 }
  action.run ctx |>.run' s |>.toIO
    fun _ => IO.Error.mkOtherError 0 "dunno"

end LSPLogAnalyzer
