import LSPLogAnalyzer
import Lean.Parser.Command

open LSPLogAnalyzer
open Lean
open ToJson
open FromJson
open JsonRpc
open Lsp
open Std
open Server.Test.Runner


#eval show _ from do
    let path := System.mkFilePath [".", "logs",
      -- "LSP_2025-11-25-16-54-08-9505+0100.log"
      -- "LSP_2025-11-27-23-08-49-7081+0100.sample.log"
      "LSP_2025-12-09-18-26-16-7165+0100.log"
      -- "logsample.log"
    ]
    let (_, st) ← processLogFile path |>.run {}
    return st

def f (path : System.FilePath): IO Unit := do
  let entries ← collectLogEntries path
  let getInteractiveGoalsIds := entries.filterMap fun e => do
    let req ← Request.ofMessage? e.msg
    guard <| req.method = "$/lean/rpc/call"
    let .ok (p : RpcCallParams) := fromJson? req.param
      | none
    guard <| p.method = `Lean.Widget.getInteractiveGoals
    return req.id
  let getInteractiveGoalsIdIndex := HashSet.ofArray getInteractiveGoalsIds
  let getInteractiveGoalsResponses := entries.filterMap fun e => do
    let resp ← Response.ofMessage? e.msg
    guard <| getInteractiveGoalsIdIndex.contains resp.id
    let .ok (p : Client.InteractiveGoals) := fromJson? resp.result
      | none
    let goals := p.goals.map (·.pretty)
    return goals.map toString |>.toList |> "\n\n".intercalate
  let fmt := getInteractiveGoalsResponses.toList |> "\n\n-----\n\n".intercalate
  IO.println fmt

#eval f (System.mkFilePath [".", "logs", "LSP_2025-11-25-16-54-08-9505+0100.log"])


-- Test for the collection of definitions
def ex_fname := "/home/ameyer/Nextcloud/Eiffel/Code/lean4/lsp_log_analyzer/Example.lean"

def ex_contents :=
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

#eval show _ from do
  let env ← prepareBaseEnv ex_fname `DummyModule
  -- IO.println $ env.constants.toList.map (·.fst)
  let defs ← runCollectDefLikes env ex_fname ex_contents
  defs.forM (IO.println ·)
  -- defs.forM (fun d : Definition => IO.println d.defview.ref)
  let .some (i, defn) := locatePos defs ⟨4, 9⟩ | return
  IO.println $ relativeRange defn.range defn.range
