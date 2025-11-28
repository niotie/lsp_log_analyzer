import Lean
open Lean Server Elab FileWorker

def parseDocument (doc : Lsp.TextDocumentItem) : CoreM Lean.Syntax := do
  let env ← getEnv
  let stx?: Except String Lean.Syntax :=
    Lean.Parser.runParserCategory env `term doc.text
  Lean.ofExcept stx?

def parseString (text : String) : MetaM Lean.Syntax := do
  let env ← getEnv
  let stx?: Except String Lean.Syntax :=
    Lean.Parser.runParserCategory env `command text
  Lean.ofExcept stx?

def myText := "
variable {p q r : Prop}

theorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by
  intro hp
  apply hqr  -- et pas hpq !
  apply hpq
  exact hp
"

def myText2 := "
theorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by
  intro hp
  apply hqr  -- et pas hpq !
  apply hpq
  exact hp
"

#eval parseString myText2

-- def collectGoals (doc : Lsp.TextDocumentItem) : IO (List (List MVarId)) := do

--   -- Build server context
--   let serverCtx ← mkInitContext {}
--   let docCtx ← FileWorker.mkInitialDocumentContext tdItem serverCtx
--   -- Compile the file, building the info tree
--   let snap ← FileWorker.compileNextCmdSnapshots docCtx
--   -- Traverse the info tree for goals
--   let rec collect (t : InfoTree) (acc : List (List MVarId)) :=
--     match t with
--     | InfoTree.node i cs =>
--       match i with
--       | Info.ofTacticInfo ti => collectGoalsInTacticInfo ti acc
--       | _ => cs.foldl (fun acc' c => collect c acc') acc
--     | InfoTree.context _ c  => collect c acc
--     | _ => acc
--   -- You have to implement collectGoalsInTacticInfo, extracting the goals from the tactic info node.
--   pure (collect snap.infoTree [])


def myInitAndRunWorker (i : IO.FS.Stream) : IO Unit := do
  let initParams ← i.readLspRequestAs "initialize" Lean.Lsp.InitializeParams
  let ⟨_, param⟩ ← i.readLspNotificationAs "textDocument/didOpen" LeanDidOpenTextDocumentParams
  let doc := param.textDocument

  let doc : DocumentMeta := {
    uri := doc.uri
    mod := ← moduleFromDocumentUri doc.uri
    version := doc.version
    -- LSP always refers to characters by (line, column),
    -- so converting CRLF to LF preserves line and column numbers.
    text := doc.text.crlfToLf.toFileMap
    dependencyBuildMode := param.dependencyBuildMode?.getD .always
  }
  let e := e.withPrefix s!"[{param.textDocument.uri}] "
  let _ ← IO.setStderr e
  let (ctx, st) ← try
    initializeWorker doc o e initParams.param opts
  catch err =>
    writeErrorDiag doc err
    throw err
  StateRefT'.run' (s := st) <| ReaderT.run (r := ctx) do
    try
      let refreshTasks ← runRefreshTasks
      mainLoop i
      for refreshTask in refreshTasks do
        refreshTask.cancel
    catch err =>
      let st ← get
      writeErrorDiag st.doc.meta err
      throw err
where
  writeErrorDiag (doc : DocumentMeta) (err : Error) : IO Unit := do
    o.writeLspMessage <| mkPublishDiagnosticsNotification doc #[{
      range := ⟨⟨0, 0⟩, ⟨1, 0⟩⟩,
      fullRange? := some ⟨⟨0, 0⟩, doc.text.utf8PosToLspPos doc.text.source.rawEndPos⟩
      severity? := DiagnosticSeverity.error
      message := err.toString }]



def myWorkerMain (logStream : IO.FS.Stream) : IO UInt32 := do
  let e ← IO.getStderr
  try
    myInitAndRunWorker logStream
    IO.Process.forceExit 0 -- Terminate all tasks of this process
  catch err =>
    e.putStrLn err.toString
    IO.Process.forceExit 1 -- Terminate all tasks of this process
