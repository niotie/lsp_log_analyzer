-- LSPLogAnalyzer/Elaboration.lean

import Lean

open Lean
open Parser
open Elab

namespace LSPLogAnalyzer

structure ElaborationState where
  inputCtx : InputContext
  parserState : ModuleParserState
  commandState : Command.State
  incrementalState : IncrementalState

/-- Initialize elaboration from empty file -/
def ElaborationState.init (env : Environment) (input fileName : String) : IO ElaborationState := do
  let inputCtx := mkInputContext input fileName
  let cmdState := Command.mkState env {} Options.empty
  let incrState ← IO.processCommandsIncrementally
    inputCtx {} cmdState none
  return {
    inputCtx,
    parserState := incrState.parserState,
    commandState := cmdState,
    incrementalState := incrState
  }

/-- Update elaboration with new file content -/
def ElaborationState.update (state : ElaborationState) (newContent : String) :
    IO (ElaborationState × PersistentArray Lean.Elab.InfoTree) := do

  let inputCtx := Parser.mkInputContext newContent "<worksheet>"
  let newIncrState ← IO.processCommandsIncrementally
    inputCtx
    state.parserState
    state.commandState
    (some state.incrementalState)

  let newState : ElaborationState := {
    inputCtx,
    parserState := newIncrState.parserState,
    commandState := newIncrState.commandState,
    incrementalState := newIncrState
  }

  return (newState, newIncrState.commandState.infoState.trees)

-- /-- Extract proof states from info trees -/
-- def extractProofStates (trees : PersistentArray Lean.Elab.InfoTree) :
--     Array (Nat × Nat × String) := by
--   let mut results := #[]
--   trees.forM fun tree => do
--     tree.forM fun info => do
--       match info with
--       | .ofTacticInfo ti =>
--         if let some range := ti.range? then
--           let line := range.start.line
--           let col := range.start.column
--           let goalsStr := Repr.repr ti.goalsBefore
--           results := results.push (line, col, goalsStr)
--       | _ => pure ()
--   results

end LSPLogAnalyzer
