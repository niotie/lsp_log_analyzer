import LSPLogAnalyzer.Definitions
import LSPLogAnalyzer.MyParser
import LSPLogAnalyzer.Utils

open Lean.Lsp

namespace LSPLogAnalyzer

def maybeUpdateDefs
    (defMap : Std.HashMap String (Array Definition))
    (defArray : Array Definition)
    : Std.HashMap String (Array Definition) :=
  defArray.foldl go defMap
  where
    go defMap newDef :=
      defMap.alter newDef.name.toString fun defs? =>
        match defs? >>= Array.back? with
        | none => some #[newDef]
        | some oldDef =>
          -- TODO : diags are ordered by position, do this more efficiently?
          if newDef.localDiags.all fun d => oldDef.localDiags.any fun d' => d == d' then
            defs?
          else
            let newStx := newDef.defview?.map (·.ref)
            let oldStx := oldDef.defview?.map (·.ref)
            if newStx == oldStx then
              defs? >>= (·.pop.push newDef)
            else
              defs? >>= (Array.push · newDef)

def mkLocalDiag (defs : Array Definition) (diag : Diagnostic)
    : TrackerM $ Option (Nat × LocalDiagnostic) := do
  let .some (i, defn) := locatePos defs diag.range.start
    | onError s!"no definition found at position {diag.range.start} in {defs}"
      return none
  let .some dr := defn.range?
    | onError s!"no range for definition {defn}"
      return none
  let localRange := relativeRange dr diag.range
  let localDiag := { diag, localRange }
  return some (i, localDiag)

def recordLocalDiags (defs : Array Definition) (diags : Array Diagnostic)
    : TrackerM (Array Definition × Array Diagnostic) :=
  diags.foldlM go (defs, #[])
  where
    go acc diag := do
      let (res, remainingDiags) := acc
      match ← mkLocalDiag defs diag with
      | none => return (res, remainingDiags.push diag)
      | some (i, ld) => return (res.modify i fun d =>
        { d with localDiags := d.localDiags.push ld }, remainingDiags)

def updateDiagnostics (uri : Uri) (diags : Array Diagnostic) : TrackerM Unit := do
  let fs ← ensureFile uri
  let .some s := fs.snapshots.back?
    | onError s!"No existing snapshot for {uri} in updateDiagnostics"
  -- if s.diags == diags then return
  let (doc, defArray, time, docHasChanged?) ← match fs.changes.back? with
  | none => pure (s.doc, fs.defArray, s.time, false)
  | some { time, version, .. } =>
    let changes := fs.changes.flatMap (·.changes)
    let newText := Lean.Server.foldDocumentChanges changes s.doc.text.toFileMap
    -- Used to be :
    -- if normalizeText newText.source == normalizeText s.doc.text
    -- TODO : check how to safely ignore text versions
    if newText.source == s.doc.text then
      pure (s.doc, fs.defArray, s.time, false)
    else
      -- TODO : avoid converting back and forth?
      let newdoc := { s.doc with version, text := newText.source }
      let defArray ← runCollectDefLikes fs.baseEnv newdoc
      pure (newdoc, defArray, time, true)
  let (defArray, globalDiags) ← recordLocalDiags defArray diags
  let s := {s with doc, time, globalDiags }
  let fs := if docHasChanged? then
    { fs with
      snapshots := fs.snapshots.push s
      defArray
      defMap := maybeUpdateDefs fs.defMap defArray
      changes := #[] }
  else
    { fs with
      snapshots := fs.snapshots.pop.push s
      defMap := maybeUpdateDefs fs.defMap defArray }
  modifyFileState uri fs

end LSPLogAnalyzer
