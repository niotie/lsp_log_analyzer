import LSPLogAnalyzer


def fileStream (filename : System.FilePath) : IO IO.FS.Stream := do
  let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
  pure (IO.FS.Stream.ofHandle handle)


def process (args : List String) : IO (List Lean.JsonRpc.Message) := do
  match args with
  | [] =>
    let stdin ← IO.getStdin
    collectMessages stdin
  | [filename] =>
    let stream ← fileStream ⟨filename⟩
    collectMessages stream
  | _ => throw (IO.userError "usage : no args or single filename")


def main (args : List String) : IO UInt32 := do
  let messages ← process args
  -- let summaries := messages.map messageSummary
  match buildDocumentStates messages with
  | .ok states =>
      IO.println states
      pure 0
  | .error e =>
      IO.println e
      pure 1

def main2 : IO Unit := do
  let input ← IO.getStdin
  let messages ← collectMessages input
  -- dumpMessages messages
  let summaries := messages.map messageSummary
  -- IO.println (Lean.Json.pretty messages.toJson)
  summaries.forM IO.println
  pure ()
