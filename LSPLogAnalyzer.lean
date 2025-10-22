import LSPLogAnalyzer.Basic
import LSPLogAnalyzer.FileState


import Lean.Data.Json
import Lean.Data.Lsp

open Lean.JsonRpc
open IO.FS


partial def collectMessages (stream : IO.FS.Stream) : IO (List Message) := do
  try
    let msg ← IO.FS.Stream.readLspMessage stream
    let tail ← collectMessages stream
    pure (msg :: tail)
  catch e =>
    if e.toString.endsWith "Stream was closed" then
      pure []
    else
      let stderr ← IO.getStderr
      stderr.putStrLn s!"{e}"
      collectMessages stream


def messageSummary : Message → String
| .request id method _params? => s!"Request: {id} {method}"
| .notification method _params? => s!"Notification: {method}"
| .response id _result => s!"Response: {id}"
| .responseError id _errCode _msg _params? => s!"Response error: {id}"

-- def dumpMessageTypes (messages : List Lean.JsonRpc.Messages)
