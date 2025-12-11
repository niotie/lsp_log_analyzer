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

def collectDefLikes (env : Environment) (fname : FilePath) (contents : String)
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


def prepareBaseEnv (fname : String) (modName : Name) : IO Environment := do
  let .some env ← runFrontend "" {} fname modName | throw $ IO.userError "unable to create base env"
  return env


def runCollectDefLikes (env : Environment) (fileName contents : String)
    : IO (Array Definition) := do
  let ctx : Context := {
    fileName := fileName,
    fileMap := FileMap.ofString "",
    snap? := none,
    cancelTk? := none }
  let s : State := { env := env, maxRecDepth := 100 }
  let action := collectDefLikes env fileName contents
  action.run ctx |>.run' s |>.toIO fun e =>
    IO.Error.mkOtherError 0 s!"error collecting definitions : {e.getRef}"

end LSPLogAnalyzer
