import Lean

open Std.Time
open Lean.Json
open Lean.ToJson
open Lean.FromJson
open Lean.JsonRpc
open Lean.Lsp
open Lean.Elab


abbrev Uri := String

instance : Lean.ToJson ZonedDateTime where
  toJson dt := dt.toISO8601String

instance : Lean.FromJson ZonedDateTime where
  fromJson?
    | .str s => ZonedDateTime.fromISO8601String s
    | _ => throw "Expected string when converting JSON to ZonedDateTime"

/-- LSP log entry (new format). -/
structure LogEntry where
  time : ZonedDateTime
  direction : MessageDirection
  kind : MessageKind
  msg : Message
  deriving Lean.FromJson, Lean.ToJson

instance : ToString LogEntry where
  toString le := pretty $ toJson le

instance : ToString MessageDirection where
  toString
  | .serverToClient => "server→client"
  | .clientToServer => "client→server"

instance : Coe LogEntry Message := ⟨LogEntry.msg⟩


/-- File change event. -/
structure ChangeEvent where
  time : ZonedDateTime
  version : Nat
  changes : Array TextDocumentContentChangeEvent

instance instMemPosRange : Membership Position Range where
  mem r p := r.start <= p ∧ p ≤ r.end

instance (p : Position) (r : Range) : Decidable (p ∈ r) := by
  rw [instMemPosRange]
  exact instDecidableAnd

instance : ToString Range where
  toString | ⟨⟨sl, sc⟩, ⟨el, ec⟩⟩ => s!"[{sl}:{sc}, {el}:{ec}]"

instance : ToString TextDocumentContentChangeEvent where
  toString ev := match ev with
  | .fullChange _ => "full text"
  | .rangeChange range text => s!"\"{text}\" at {range}"

instance : ToString ChangeEvent where
  toString ev :=
    let ranges := ev.changes.map toString
    s!"[{ev.time} - version {ev.version}] " ++ ", ".intercalate ranges.toList


namespace Lean.Elab.DefView

  def getLspRange (dv : DefView) (inCtx : Parser.InputContext) : Lsp.Range :=
    let range := dv.ref.getRange?.get!
    inCtx.fileMap.utf8RangeToLspRange range

  def getId (dv : DefView) : Name :=
    match dv.declId.find? (·.isIdent) with
    | none => .anonymous
    | some id => id.getId

  instance : ToString DefKind where
    toString
    | .theorem => "theorem"
    | .abbrev => "abbrev"
    | .opaque => "opaque"
    | .example => "example"
    | .instance => "instance"
    | .def => "def"

  instance : ToString DefView where
    toString | dv@{ kind, .. } => s!"{kind} {dv.getId}"

end Lean.Elab.DefView

instance : ToString DiagnosticSeverity where
  toString
  | .information => "info"
  | .error => "error"
  | .warning => "warning"
  | .hint => "hint"

instance : ToString Diagnostic where
  toString diag :=
    let s := match diag.severity? with
    | some s => s!" ({s})"
    | none => ""
    s!"{diag.range}{s} {diag.message}"

structure LocalDiagnostic where
  localRange : Range
  diag : Diagnostic

instance : ToString LocalDiagnostic where
  toString ldiag :=
    toString { ldiag.diag with range := ldiag.localRange }

instance : BEq LocalDiagnostic where
  beq ld ld' :=
    ld.localRange == ld'.localRange && ld.diag.message == ld'.diag.message
    -- { ld.diag with range := ld.localRange } == { ld'.diag with range := ld'.localRange }

/-- File snapshot. -/
structure Definition where
  name : Lean.Name
  version : Nat
  kind : DefKind
  range? : Option Range
  defview? : Option DefView
  localDiags : Array LocalDiagnostic := #[]

instance : ToString Definition where
  toString
  | { name, version, kind, range?, localDiags, .. } =>
    let r := match range? with
    | none => "no definition in file"
    | some r => s!"range {r}"
    let ds := match localDiags with
    | #[] => ""
    | _ => s!"\n{localDiags.size} local diagnostics:\n\
            {"\n".intercalate (localDiags.toList.map toString)}"
    s!"{kind} {name} (v{version}): {r}{ds}"

/-- File snapshot. -/
structure Snapshot where
  time : ZonedDateTime
  doc : TextDocumentItem
  -- goals : Array Lean.Widget.InteractiveGoal  -- not used yet
  globalDiags : Array Diagnostic
  deflikes : Array Definition

instance : ToString DiagnosticSeverity where
  toString
  | .information => "info"
  | .error => "error"
  | .warning => "warning"
  | .hint => "hint"

instance : ToString Diagnostic where
  toString diag :=
    let s := match diag.severity? with
    | some s => s!" ({s})"
    | none => ""
    s!"{diag.range}{s} {diag.message}"

instance : ToString Snapshot where
  toString snap :=
    let v := snap.doc.version
    let deflikes := match snap.deflikes with
    | #[] => ""
    | _ => s!"[definitions]\n{String.intercalate "\n" $ snap.deflikes.toList.map toString}\n"
    let diags := match snap.globalDiags with
    | #[] => ""
    | _ => s!"[global diagnostics]\n{String.intercalate "\n" $ snap.globalDiags.toList.map toString}\n"
    s!"[begin version {v} ({snap.time})]\n\
      {snap.doc.text.trim}\n{diags}{deflikes}[end version {v}]"


/-- Per-file replay state. -/
structure FileState where
  baseEnv : Lean.Environment
  defMap : Std.HashMap String (Array Definition) := {}
  defArray : Array Definition := #[]
  snapshots : Array Snapshot := #[]
  changes : Array ChangeEvent := #[]

instance : ToString FileState where
  toString fs :=
    let ssnaps := String.intercalate "\n\n" $ fs.snapshots.toList.map toString
    let sdefs := match fs.defMap.size with
    | 0 => ""
    | _ =>
      let tmp := fs.defMap.values.map
        fun da : Array Definition => da.toList.map toString
      let strings := tmp.map ("\n".intercalate)
      s!"[definitions]\n{"\n".intercalate strings}\n"
    let schanges := match fs.changes with
    | #[] => ""
    | _ => "Pending changes:\n" ++ (String.intercalate "\n" $ fs.changes.toList.map toString)
    s!"Recorded snapshots:\n{ssnaps}\n{sdefs}\n{schanges}"

/-- Log entry tracking error. -/
structure TrackingError where
  message : String
  -- entry : LogEntry

instance : ToString TrackingError where
  toString te := te.message


/-- Global tracker. -/
structure Tracker where
  files : Std.HashMap Uri FileState := {}
  errors : Array TrackingError := #[]
  line : Nat := 1
  requests : Std.HashMap RequestID (Request Lean.Json) := {}
deriving Inhabited

instance : ToString Tracker where
  toString tracker :=
    let fss := "\n\n".intercalate $ tracker.files.toList.map
      fun (uri, fs) => s!"State for {uri}:\n\n{fs}"
    let errors := "\n".intercalate $ tracker.errors.toList.map toString
    let requests := "\n".intercalate $ tracker.requests.toList.map
      fun (id, req) =>
        let m := match req.method with
        | "$/lean/rpc/call" =>
            match fromJson? req.param with
            | .ok (param : RpcCallParams) => s!"{req.method} ({param.method})"
            | _ => req.method
        | _ => req.method
        s!"{id}:\t{m}"
    s!"{fss}\nErrors:\n{errors}\n\nPending requests:\n{requests}"

/-- Tracker monad. -/
abbrev TrackerM := StateT Tracker IO
