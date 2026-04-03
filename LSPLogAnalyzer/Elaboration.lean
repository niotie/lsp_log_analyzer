-- LSPLogAnalyzer/Elaboration.lean

import Lean
import LSPLogAnalyzer.Definitions
import LSPLogAnalyzer.Utils

open Lean
open Parser
open Elab

namespace LSPLogAnalyzer

def initializeBaseEnvironment : TrackerM Unit := do
  try
    let env ← importModules
      (imports := #[])
      (opts := Elab.async.set Options.empty false)
      (trustLevel := 1024)
    modify fun st => { st with baseEnv := some env }
  catch e =>
    onError s!"Failed to initialize base environment: {e}"

/-- Initialize elaboration from provided file -/
def initElabState (content fileName : String)
    : IO IncrementalState := do
  let inputCtx := mkInputContext content fileName
  let opts := Elab.async.set Options.empty false
  -- enableInitializersExecution
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← processHeader header opts messages inputCtx
  let commandState := Command.mkState env messages opts
  return ← IO.processCommandsIncrementally
    inputCtx parserState commandState none
  -- return {
  --   inputCtx := incrState.inputCtx,
  --   parserState := incrState.parserState,
  --   commandState := incrState.commandState,
  --   incrementalState := incrState
  -- }

-- /-- Update elaboration with new file content -/
-- def updateElabState (state : IncrementalState) (newContent fileName : String) :
--     IO IncrementalState := do
--   let inputCtx := Parser.mkInputContext newContent fileName
--   let newIncrState ← IO.processCommandsIncrementally
--     inputCtx
--     state.parserState
--     state.commandState
--     (some state.incrementalState)
--   return {
--     inputCtx,
--     parserState := newIncrState.parserState,
--     commandState := newIncrState.commandState,
--     incrementalState := newIncrState
--   }

/-- Is the `Syntax` for this `Lean.Elab.Info` original, or synthetic?
    Adapted from https://github.com/kim-em/lean-training-data. -/
def isOriginal (info : Info) :=
  match info.stx.getHeadInfo with
  | .original .. => true
  | _ => false

/-- Is the `Syntax` for this `Lean.Elab.Info` original, or synthetic?
    Adapted from https://github.com/kim-em/lean-training-data. -/
def isTacticInfo (info : Info) :=
  match info with
  | .ofTacticInfo .. => true
  | _ => false

/-- Find the name for the outermost `Syntax` in this `Info`.
    Stolen from https://github.com/kim-em/lean-training-data. -/
def name? (info : Info) : Option Name :=
  match info.stx with
  | Syntax.node _ n _ => some n
  | _ => none

/-- Decide whether a tactic is "substantial", or is merely a tactic combinator
    (e.g. `by`, `;`, multiline tactics, parenthesized tactics).
    Stolen from https://github.com/kim-em/lean-training-data. -/
def isSubstantial (t : Info) : Bool :=
  match name? t with
  | none => false
  | `null => false
  | ``cdot => false
  | ``cdotTk => false
  | ``Lean.Parser.Term.byTactic => false
  | ``Lean.Parser.Tactic.tacticSeq => false
  | ``Lean.Parser.Tactic.tacticSeq1Indented => false
  | ``Lean.Parser.Tactic.«tactic_<;>_» => false
  | ``Lean.Parser.Tactic.paren => false
  | _ => true

def defaultFilter (info : Info) : Bool :=
  isOriginal info && isTacticInfo info && isSubstantial info

def posAndGoalsBefore (ctx : ContextInfo) (info : Info) := do
  let .ofTacticInfo ti := info | none
  let range ← ti.stx.getRange?
  let ctx := { ctx with mctx := ti.mctxBefore }
  return (range.start, range.stop, ctx, ti.goalsBefore)

def posAndGoalsAfter (ctx : ContextInfo) (info : Info) := do
  let .ofTacticInfo ti := info | none
  let range ← ti.stx.getRange?
  let ctx := { ctx with mctx := ti.mctxAfter }
  return (range.start, range.stop, ctx, ti.goalsAfter)

/-- Collect proof states from elaboration state -/
def extractGoals
    (state : IncrementalState)
    (filter := defaultFilter)
    : List (String.Pos.Raw × String.Pos.Raw × ContextInfo × List (MVarId)) :=
  trees.map collect |>.toList |>.flatten
  where
    trees := state.commandState.infoState.trees
    collect (tree : Lean.Elab.InfoTree) :=
      Id.run (tree.visitM (postNode :=
        fun ctx info children childResults =>
          let childResultsFlat := childResults.filterMap id |>.flatten
          if filter info then
            let new := if children.isEmpty
              then posAndGoalsAfter ctx info
              else posAndGoalsBefore ctx info
            -- TODO : ugly
            match new with
            | .some new => new :: childResultsFlat
            | none => childResultsFlat
          else
            childResultsFlat
      )) |>.getD []

def ppState
    (state : IncrementalState)
    (filter : Info → Bool := defaultFilter)
    : IO Unit := do
  let trees := state.commandState.infoState.trees
  -- Print unreported messages
  if state.commandState.messages.toList.length > 0 then
    IO.println "Unreported messages:"
    state.commandState.messages.forM (fun msg => do
      IO.println s!"{← msg.toString}")
  -- Print tactic info
  let postNode ctx info _ := do
    if filter info then IO.println (← info.format ctx)
  let collect (tree : Lean.Elab.InfoTree) := do
    tree.visitM' (postNode := postNode)
  trees.forM collect

end LSPLogAnalyzer
