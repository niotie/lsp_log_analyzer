import Lean
import LSPLogAnalyzer

open Lean
open Parser
open LSPLogAnalyzer

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

-- def testParseModule (fname contents : String) : TrackerM  (TSyntax `Lean.Parser.Module.header × ModuleParserState × MessageLog) := do
--   let inputCtx := mkInputContext contents fname
--   let (header, state, messages) ← parseHeader inputCtx
--   return (header, state, messages)

  -- let cmds ← testParseModuleAux env inputCtx state messages #[]
  -- let stx := mkNode `Lean.Parser.Module.module #[header, mkListNode cmds]
  -- pure ⟨stx.raw.updateLeading⟩

open Command

#eval show Elab.Command.CommandElabM _ from do
  let fname := "/home/ameyer/Nextcloud/Eiffel/Code/lean4/lsp_log_analyzer/Example.lean"
  let content :=
"variable {p q r : Prop}

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
  let env ← getEnv
  -- IO.println env.mainModule
  let mod ← testParseModule env fname content
  -- IO.println mod.raw
  let ctx := mkInputContext content fname
  let options ← getOptions
  let pmCtx := { env, options }
  let (_, mps, mlog) := parseCommand ctx pmCtx {} {}
  let (s, _, _) := parseCommand ctx pmCtx mps mlog
  let d ← Elab.Command.mkDefView {} s[1]
  let { start, stop } := s.getRange?.get!
  return (d.declId, ctx.fileMap.toPosition start, ctx.fileMap.toPosition stop)


#check Elab.Term.getDeclName?

#check Elab.DefView
