import Lean.Server

open Lean.JsonRpc
open Lean.FromJson
open Lean.Lsp
open Lean.Server

def documentStateEvents := ["textDocument/didOpen", "textDocument/didChange"]

def isRelevant (message : Message) : Bool :=
  match message with
  | .notification method (some (.obj _params)) =>
        method ∈ documentStateEvents
  | _ => False

structure DocumentState where
  version : Option Int
  text : Lean.FileMap

instance : ToString DocumentState where
  toString := fun ⟨version?, ⟨text, _⟩⟩ =>
    let vstring := match version? with
    | none => ""
    | some v => s!" (version {v})"
    s!"---begin{vstring}---\n{text}\n---end---\n\n"

abbrev DocumentStates :=
  Std.TreeMap DocumentUri (List DocumentState)

instance : ToString DocumentStates where
  toString ds :=
    let step s uri states :=
      s ++ s!"{uri}\n" ++ String.join (states.reverse.map toString)
    ds.foldl step ""

def updateDocumentStates (docstates : DocumentStates) (message : Message) :
    Except String DocumentStates := do
  match message with
  | .notification "textDocument/didOpen" (some (.obj params)) =>
      let document : TextDocumentItem ← fromJson? (params.get! "textDocument")
      let uri := document.uri
      let docstate : DocumentState := ⟨some document.version, document.text.toFileMap⟩
      pure (docstates.insert uri [docstate])
  | .notification "textDocument/didChange" (some (.obj params)) =>
      let doc : VersionedTextDocumentIdentifier ← fromJson? (params.get! "textDocument")
      let statelist : List DocumentState := docstates.get! doc.uri
      let changeEvents ← fromJson? (params.get! "contentChanges")
      let currentText ← match statelist.head? with
      | none => Except.error s!"no current state for file {doc.uri}"
      | some ⟨_, text⟩ => Except.ok text
      let newText := foldDocumentChanges changeEvents currentText
      let newState : DocumentState := ⟨doc.version?, newText⟩
      pure (docstates.modify doc.uri (fun _ => (newState :: statelist)))
  | _ => Except.error s!"message is not relevant to document state"

def buildDocumentStates (messages : List Message) : Except String DocumentStates :=
  let relevantMsgs := messages.filter isRelevant
  relevantMsgs.foldlM  updateDocumentStates Std.TreeMap.empty
