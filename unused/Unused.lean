import Lean

open Lean.JsonRpc

partial def processLines
    (stream : IO.FS.Stream)
    (processor : String → Except String α)
    : IO (List α) := do
  match (← stream.getLine) with
  | "" => pure []
  | line =>
      let head := processor line
      let tail ← processLines stream processor
      match head with
      | .error e =>
          (← IO.getStderr).putStrLn e
          return tail
      | .ok entry =>
          return entry :: tail

def getUri (msg : Message) : Option String :=
    match msg with
    | .request _ _ (some params)
    | .notification _ (some params) =>
        match do
          let params := params.toJson
          let textDoc ← params.getObjVal? "textDocument"
          let uri ← textDoc.getObjVal? "uri"
          uri.getStr?
        with
        | .ok s => some s
        | _ => none
    | _ => none

def exampleMessageFilter (msg : Message) : Bool :=
  match msg with
  | .request _ "initialize" .. => true
  | .notification "textDocument/didOpen" ..
  | .notification "textDocument/didChange" .. =>
    match getUri msg with
    | none => false
    | some s => s.endsWith "Example.lean"
  | _ => false


open IO FS in
partial def collectMessages (stream : Stream) (filter : Message → Bool) : IO (List Message) := do
  try
    let msg ← stream.readLspMessage
    let tail ← collectMessages stream filter
    if filter msg then
      pure (msg :: tail)
    else
      pure tail
  catch e =>
    if e.toString.endsWith "Stream was closed" then
      pure []
    else
      let stderr ← getStderr
      stderr.putStrLn s!"{e}"
      collectMessages stream filter

-- partial def collectLogEntries (stream : IO.FS.Stream) (filter : Message → Bool) : IO (List LogEntry) := do
--   match ← stream.getLine with
--   | "" => pure []
--   | line =>
--       let j ← .ofExcept $ parse line
--       let entry : Except String LogEntry := Lean.fromJson? j
--       match entry with
--       | .error e =>
--           (← IO.getStderr).putStrLn e
--           tail
--       | .ok entry =>
--           if filter entry then
--             return entry :: (← tail)
--           else
--             tail
--       where tail := collectLogEntries stream filter
