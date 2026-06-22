-- Analyzer/Main.lean

import Lean
import Lean.Elab.Frontend

structure ReplayState where
  inputCtx : Parser.InputContext
  parserState : Parser.ModuleParserState
  commandState : Lean.Elab.Command.State
  incrementalState : Lean.Elab.Frontend.IncrementalState

def updateFile (state : ReplayState) (newContent : String) :
    IO (ReplayState × PersistentArray Lean.Elab.InfoTree) := do

  let inputCtx := Parser.mkInputContext newContent "<worksheet>"

  let newIncrState ← Lean.Elab.Frontend.IO.processCommandsIncrementally
    inputCtx
    state.parserState
    state.commandState
    (some state.incrementalState)

  let newState : ReplayState := {
    inputCtx,
    parserState := newIncrState.parserState,
    commandState := newIncrState.commandState,
    incrementalState := newIncrState
  }

  return (newState, newIncrState.commandState.infoState.trees)

def processEvent (state : ReplayState) (event : Json) :
    IO (ReplayState × Option (PersistentArray Lean.Elab.InfoTree)) := do

  match event.getObjVal? "method" with
  | some (Json.str "textDocument/didOpen") =>
    let text := event.getObjVal? "params"
      |> Option.bind (·.getObjVal? "textDocument")
      |> Option.bind (·.getObjVal? "text")
      |> Option.bind Json.asStr
      |> Option.getD ""

    let (newState, trees) ← updateFile state text
    return (newState, some trees)

  | some (Json.str "textDocument/didChange") =>
    -- For simplicity: assume full document change
    -- (or implement LSP range application if needed)
    let text := event.getObjVal? "params"
      |> Option.bind (·.getObjVal? "contentChanges")
      |> Option.bind (·.asArray?.map (·[0]?))
      |> Option.bind (·.flatMap Json.asStr)
      |> Option.getD ""

    let (newState, trees) ← updateFile state text
    return (newState, some trees)

  | _ =>
    return (state, none)

def main : IO Unit := do
  -- Initialize environment
  let env ← importModules
    (imports := [])
    (opts := Options.empty)
    (trustLevel := 1024)

  let initialInputCtx := Parser.mkInputContext "" "<worksheet>"
  let initialCmdState := Lean.Elab.Command.mkState env {} Options.empty

  let initialIncrState ← Lean.Elab.Frontend.IO.processCommandsIncrementally
    initialInputCtx {} initialCmdState none

  let mut state : ReplayState := {
    inputCtx := initialInputCtx,
    parserState := {},
    commandState := initialCmdState,
    incrementalState := initialIncrState
  }

  -- Read JSON log line by line from stdin
  let stdin ← IO.getStdin
  while true do
    match ← stdin.getLine with
    | "" => break
    | line =>
      match Json.parse? line with
      | .error _ => continue
      | .ok event =>
        let (newState, treesOpt) ← processEvent state event
        state := newState

        -- Do whatever you want with trees here
        match treesOpt with
        | some trees =>
          -- Example: dump proof states
          trees.forM fun tree => do
            tree.forM fun info => do
              match info with
              | .ofTacticInfo ti =>
                if let some range := ti.range? then
                  IO.println s!"{range.start.line}:{range.start.column} {Repr.repr ti.goalsBefore}"
              | _ => pure ()
        | none => pure ()

#eval main (← IO.getArgs)
