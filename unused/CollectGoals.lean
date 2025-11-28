/- Example: RequestM handler that builds a DocumentMeta from a TextDocumentItem,
   applies didChange-style edits, asks the FileWorker pipeline to reprocess the
   document, and collects the `Elab.Info` tactic nodes (which contain the goals).

   This is a skeleton that shows the exact pieces you need and how to traverse
   the resulting info tree to extract tactic info / goals. You may need to
   adjust a few FileWorker helper names depending on your Lean4 checkout / version.

   Usage:
   - Drop this file into a file in a project that depends on `lean4` (server code).
   - Integrate the `handleCollectGoals` function into a RequestM handler.
-/

import Lean
import Lean.Elab
import Lean.Elab.InfoTree
import Lean.Server.Utils
import Lean.Server.FileWorker
import Lean.Data.Lsp.Basic

open Lean
open Lean.Elab
open Lean.Elab.InfoTree
open Lean.Server
open Lean.Lsp

/-- Convert a TextDocumentItem -> DocumentMeta (normalizing CRLF). -/
def makeDocumentMeta (td : TextDocumentItem) : DocumentMeta := {
  version := td.version,
  uri := td.uri,
  mod := ← moduleFromDocumentUri td.uri,
  text    := td.text.crlfToLf.toFileMap,
  dependencyBuildMode := default -- choose appropriate mode
}

/-- Traverse an InfoTree and collect all `Elab.Info` tactic nodes (Info.ofTacticInfo). -/
partial def collectTacticInfosFromInfoTree (t : InfoTree) : List Elab.Info := Id.run do
  let mut acc : List Elab.Info := []
  -- helper recursive traversal
  let rec go (t : InfoTree) : Unit :=
    match t with
    | InfoTree.node i children =>
      match i with
      | Info.ofTacticInfo ti => acc := ti :: acc
      | _ => ()
      for c in children do go c
    | InfoTree.context _ child => go child
    | InfoTree.nil => ()
  go t
  acc

/-- Given a TextDocumentItem and an (optional) list of content changes,
    reprocess the document using the FileWorker pipeline and return all tactic infos.
    This runs in RequestM (the same monad used by server handlers). -/
def handleCollectGoals (td : TextDocumentItem) (changes? : Option (Array TextDocumentContentChangeEvent))
    : RequestM (Array Elab.Info) := do
  -- 1) Build DocumentMeta
  let docMeta := makeDocumentMeta td

  -- 2) If there are didChange edits, apply them to the FileMap
  let newFileMap :=
    match changes? with
    | some chs => Lean.Server.Utils.foldDocumentChanges chs docMeta.text
    | none     => docMeta.text

  let updatedMeta : DocumentMeta := { docMeta with text := newFileMap }

  /-
    3) Insert the document into the server's document store / create an EditableDocument
       and hand it to the FileWorker pipeline for parsing & elaboration.

    The FileWorker API exposes helpers that:
      - create an EditableDocument or EditableDocumentCore from a DocumentMeta
      - run the per-command parsing/elaboration pipeline producing "snapshots" and an InfoTree

    Below we call those helpers. The exact helper name used in your checkout may vary:
      - `FileWorker.mkEditableDocument` / `FileWorker.EditableDocument.mk` (construct an editable doc)
      - `FileWorker.mkCmdSnaps` / `FileWorker.mkCmdSnaps_go` or similar to produce command snapshots
      - `FileWorker.processDocument` / `FileWorker.compileNextCmdSnapshots` etc. to actually run the pipeline

    Replace the placeholders below with the concrete names in your version if necessary.
  -/

  -- 3a) Construct an EditableDocument for the FileWorker pipeline
  let editableDoc ← FileWorker.EditableDocument.mkFromDocumentMeta td.uri updatedMeta

  -- 3b) Run the FileWorker pipeline to (re)parse and (re)elaborate the document.
  --     This returns a structure that contains the InfoTree (and snapshots).
  let fileResult ← FileWorker.processDocument editableDoc

  -- 4) Extract the InfoTree from the result and traverse it
  let infoTree : InfoTree := fileResult.infoTree

  let tacticInfos := collectTacticInfosFromInfoTree infoTree

  -- 5) Return the collected info nodes as an array
  pure tacticInfos.toArray

/-
Notes / explanation:

- Monad choice:
  - RequestM is the right monad when you are implementing server-side handlers that
    interact with the FileWorker pipeline and the server document store. It wraps the
    server context and gives access to the server state and environment used by the pipeline.
  - CoreM / Elab.Command / TermElab are used for local parsing/elaboration tasks (single snippet).
    For document-level incremental parsing + elaboration you want RequestM (or ServerM inside the server).

- Key helpers:
  - makeDocumentMeta: build a DocumentMeta from a TextDocumentItem (normalizes CRLF → LF).
  - Lean.Server.Utils.foldDocumentChanges: apply LSP didChange edits to an existing FileMap.
  - FileWorker.EditableDocument.* and FileWorker.processDocument (placeholder names):
    the FileWorker module contains utilities to build an "editable document", run the incremental
    pipeline and return snapshots + an InfoTree. Use the concrete names from your checkout:
      - look for `EditableDocument`, `mkCmdSnaps`, `mkCmdSnaps_go`, `processDocument`,
        `compileNextCmdSnapshots` inside src/Lean/Server/FileWorker.lean.

- InfoTree traversal:
  - The InfoTree produced by the pipeline contains `Info.ofTacticInfo` nodes for tactic steps;
    these hold the elaboration/tactic data needed to build goal views.
  - The example `collectTacticInfosFromInfoTree` recursively collects those nodes.

If you paste this into a server plugin / RequestM handler and replace the FileWorker helper placeholders
with the exact function names in your Lean checkout, it will give you the full elaboration info
(including goals) produced incrementally for the provided document.
-/
