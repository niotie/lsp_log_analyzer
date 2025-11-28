/- Student stats: RequestM handler that inspects the in-memory EditableDocument for an open file,
   finds the snapshot(s) covering a position, and returns the current goals (pretty‑printed
   strings) found in the info tree.

   Target: Lean 4.24.0. This file is a compact, copy‑able example you can drop into a server
   plugin module. It reuses the server data structures: EditableDocument, cmdSnaps, InfoTree.

   NOTES:
   - The code uses the same APIs the server handlers use (readDoc, doc.cmdSnaps.getFinishedPrefixWithTimeout,
     InfoTree.goalsAt?).
   - Some small helper/pretty-print functions for goals may need to be adjusted to the exact API in your
     checkout (I left small comments where adaptation may be necessary).
-/

import Lean
import Lean.Elab
import Lean.Elab.InfoTree
import Lean.Server.FileWorker
-- import Lean.Server.RequestHandling
import Lean.Data.Lsp.Extra

open Lean
open Elab
open Server
open Lsp
-- open Extra

/-- Convert an Elab goal structure to a user-visible String.
    Adapt this helper if your Lean checkout exposes a different pretty-print function. -/
private def goalToString (text : FileMap) (g : Goal) : String :=
  -- The Info/Goal types provide pretty-printing used by the widgets. If your checkout exposes
  -- a `pretty`/`toString`/`fmt` accessor for goals, use that here. As a conservative fallback we
  -- render some fields cheaply.
  match g.pretty? with
  | some fmt => toString fmt
  | none =>
    -- fallback: show the goal's goalType and local context size (if available)
    let tp := g.mvarDecl?.bind (fun d => d.type?) -- optional
    match tp with
    | some t => toString t
    | none   => s!"<goal>"

-- Request handler that returns PlainGoal for a given document position.
def handleCollectPlainGoal (params : PlainGoalParams) : RequestM (LspResponse (Option PlainGoal)) := do
  let ctx ← read
  let doc ← readDoc
  let text := doc.meta.text
  let hoverPos := text.lspPosToUtf8Pos params.position

  -- Ask for the finished prefix of command snapshots (timeout in ms).
  -- This mirrors how the server builds responses incrementally and avoids waiting forever.
  let (snaps, _st_more, _isComplete) ←
    doc.cmdSnaps.getFinishedPrefixWithTimeout 3000 (cancelTks := ctx.cancelTk.cancellationTasks)

  -- Search the finished snapshots for the first snapshot that covers the hover position
  -- and contains an InfoTree with goals.
  for snap in snaps do
    -- `snap` has fields like `endPos`, `stx?`, `infoTree?` depending on your checkout.
    if snap.endPos < hoverPos then
      continue
    -- Only consider snapshots that have an infoTree available
    match snap.infoTree? with
    | none => continue
    | some infoTree =>
      -- `InfoTree.goalsAt?` is the utility the server uses to gather goals at a position.
      match infoTree.goalsAt? text params.position with
      | none => continue
      | some goals =>
        -- Convert goals to strings (pretty-print). Adjust `goalToString` if necessary.
        let goalsStrs := goals.map (fun g => goalToString text g)
        let rendered := goalsStrs.foldl (· ++ ·) "" -- simple concatenation
        let pg : PlainGoal := { rendered := rendered, goals := goalsStrs.toArray }
        return mkResponse pg

  -- If no goals found, return "no goals" style result with empty list
  let emptyPg : PlainGoal := { rendered := "no goals", goals := #[] }
  return mkResponse emptyPg

/-
What this file does (summary of progress):
- I implemented a RequestM handler `handleCollectPlainGoal` that:
  * reads the server `RequestM` context and the current in-memory `EditableDocument` (`readDoc`);
  * converts the LSP position to the document's UTF-8 position;
  * asks for the finished prefix of command snapshots (with a short timeout) via the same
    `cmdSnaps` API the server uses for other features (semantic tokens, etc.);
  * traverses those snapshots and, for the first snapshot that covers the position and contains
    an InfoTree, uses `InfoTree.goalsAt?` to obtain the goals at the position;
  * pretty-prints the goals to strings and returns them in the `PlainGoal` response type defined
    in the server's LSP extensions.

Key reuse and why this is safe:
- This handler reuses the server's canonical in-memory state (EditableDocument and its cmdSnaps)
  and the InfoTree produced by the elaborator. That means you do not reparse or re-elaborate code;
  you read the already-built results the worker maintains.
- The approach is non-blocking in practice: we use the same `getFinishedPrefixWithTimeout` pattern
  used by the server to build quick responses without waiting indefinitely for long-running tasks.

Small adaptation notes (things to verify in your checkout):
- `snap.infoTree?` and `infoTree.goalsAt?` names are the ones used by the server; if your exact
  Lean 4.24.0 build exposes slightly different names, replace them accordingly.
- `Elab.Goal.pretty?` was used conservatively above; if the goal type offers a direct pretty-printer
  (or a `toString`/`fmt` field), swap it into `goalToString`.
- `mkResponse` builds an `LspResponse` value; if your server helper expects the handler to return
  a `RequestTask` or a different wrapper, wrap the result as in other server handler examples.

Why this is a good starting point for student analytics:
- You get exact per-position goal snapshots that the student sees in the editor (goals, hypotheses,
  metavariable contexts) without re-running anything.
- By invoking this handler at didOpen/didChange or periodically, you can collect a stream of
  timestamped PlainGoal events for aggregation (store only the small serialized strings or hashed
  representations if you need to anonymize).

- End of file.
-/
