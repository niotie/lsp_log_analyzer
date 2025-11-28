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
