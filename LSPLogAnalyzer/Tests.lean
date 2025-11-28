import LSPLogAnalyzer

open LSPLogAnalyzer
open Lean.ToJson
open Lean.FromJson
open Lean.JsonRpc
open Lean.Lsp
open Std
open Lean.Server.Test.Runner

def noFilter (_ : α) := true

#eval show _ from do
    let path := System.mkFilePath [".", "logs", "LSP_2025-11-25-16-54-08-9505+0100.log"]
    let entries ← collectLogEntries' path
    let (_, st) ← entries.forM processLogEntry |>.run {}
    return st

def f (path : System.FilePath): IO Unit := do
  let entries ← collectLogEntries' path
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
