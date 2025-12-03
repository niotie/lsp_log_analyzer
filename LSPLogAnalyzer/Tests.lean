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
      -- "LSP_2025-12-03-14-26-00-5626+0100.log"
      "logsample.log"
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


open Parser

-- partial def testParseModuleAux (inputCtx : InputContext) (s : ModuleParserState) (msgs : MessageLog) (stxs  : Array Syntax) : IO (Array Syntax) :=
--   let rec parse (state : ModuleParserState) (msgs : MessageLog) (stxs : Array Syntax) :=
--     match parseCommand inputCtx { env := env, options := {} } state msgs with
--     | (stx, state, msgs) =>
--       if isTerminalCommand stx then
--         if !msgs.hasUnreported then
--           pure stxs
--         else do
--           msgs.forM fun msg => msg.toString >>= IO.println
--           throw (IO.userError "failed to parse file")
--       else
--         parse state msgs (stxs.push stx)
--   parse s msgs stxs

def testParseModule (fname contents : String) : TrackerM  (TSyntax `Lean.Parser.Module.header × ModuleParserState × MessageLog) := do
  let inputCtx := mkInputContext contents fname
  let (header, state, messages) ← parseHeader inputCtx
  return (header, state, messages)

  -- let cmds ← testParseModuleAux env inputCtx state messages #[]
  -- let stx := mkNode `Lean.Parser.Module.module #[header, mkListNode cmds]
  -- pure ⟨stx.raw.updateLeading⟩

#eval show _ from do
  let ((synt, _, _), s) ← testParseModule "/home/ameyer/Nextcloud/Eiffel/Code/lean4/lsp_log_analyzer/Example.lean" "variable {p q r : Prop}

  theorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by
    intro hp
    exact hqr (hpq hp)

  theorem or_comm' (h : p ∨ q) : q ∨ p := by
    rcases h with h | h
    right
    exact h
    left
    exact h
  " |>.run {}
  return synt
