import Lean
import LSPLogAnalyzer

open Lean
open Parser
open Parser.Command
open Elab
open Elab.Command
open System
open LSPLogAnalyzer

open Std

namespace Lean.Elab.DefView

  def getLspRange (dv : DefView) (inCtx : InputContext) : Option Lsp.Range :=
    dv.ref.getRange?.map inCtx.fileMap.utf8RangeToLspRange

  def getId (dv : DefView) : Name :=
    match dv.declId.find? (·.isIdent) with
    | none => .anonymous
    | some id => id.getId

  instance : ToString DefKind where
    toString
    | .theorem => "theorem"
    | .abbrev => "abbrev"
    | .opaque => "opaque"
    | .example => "example"
    | .instance => "instance"
    | .def => "def"

  instance : ToString DefView where
    toString | dv@{ kind, .. } => s!"{kind} {dv.getId}"

end Lean.Elab.DefView

def collectDefLikes (fname : FilePath) (contents : String)
    : CommandElabM (Array (DefView × Option Lsp.Range)) := do
  let env ← getEnv -- FIXME : write mkFreshEnv
  let inCtx := mkInputContext contents fname.toString
  let pmCtx := { env, options := {} }
  let (headerStx, mps, msgl) ← parseHeader inCtx
  -- TODO : elab the header, not forgetting to update env

  let mut mps := mps
  let mut msgl := msgl
  let mut res := #[]
  let mut cmdStx := .missing

  repeat
    (cmdStx, mps, msgl) := parseCommand inCtx pmCtx mps msgl
    -- let _ ← elabCommand cmdStx  -- TODO : necessary ?
    if cmdStx.getNumArgs > 1 && isDefLike cmdStx[1] then
      let defView ← mkDefView {} cmdStx[1]
      res := res.push (defView, defView.getLspRange inCtx)
  until isTerminalCommand cmdStx

  return res

#eval show Elab.Command.CommandElabM _ from do
  let fname := "/home/ameyer/Nextcloud/Eiffel/Code/lean4/lsp_log_analyzer/Example.lean"
  let contents :=
"import Lean

variable {p q r : Prop}

theorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by
  intro hp
  exact hqr (hpq hp)

theorem or_comm' (h : p ∨ q) : q ∨ p := by
  rcases h with h | h
  right
  exact h
  left
  exact h
"
  let defs ← collectDefLikes fname contents
  defs.forM (fun
  | (dv, .some r) => IO.println s!"{dv} {r}"
  | (dv, .none) => IO.println s!"{dv} (no position)")
