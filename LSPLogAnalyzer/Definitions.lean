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

/-- File snapshot. -/
structure Definition where
  name : Lean.Name
  range : Range
  defview : DefView
  diags : Array Diagnostic := #[]

instance : ToString Definition where
  toString
  | { name, range, defview, .. } => s!"{defview.kind} {range} {name}"


/-- File snapshot. -/
structure Snapshot where
  time : ZonedDateTime
  doc : TextDocumentItem
  goals : Array Lean.Widget.InteractiveGoal  -- not used yet
  diags : Array Diagnostic
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
    let diags := match snap.diags with
    | #[] => ""
    | _ => s!"[diagnostics]\n{String.intercalate "\n" $ snap.diags.toList.map toString}\n"
    let deflikes := match snap.deflikes with
    | #[] => ""
    | _ => s!"[definitions]\n{String.intercalate "\n" $ snap.deflikes.toList.map toString}\n"
    s!"[begin version {v} ({snap.time})]\n\
      {snap.doc.text.trim}\n\
      {diags}\
      {deflikes}\
      [end version {v}]"


/-- Per-file replay state. -/
structure FileState where
  snapshots : Array Snapshot := #[]
  changes : Array ChangeEvent := #[]
  currentSnap : Option Snapshot := none   -- not used yet
  currentDiags : Array Diagnostic := #[]  -- not used yet
deriving Inhabited

instance : ToString FileState where
  toString fs :=
    let ssnaps := String.intercalate "\n\n" $ fs.snapshots.toList.map toString
    let schanges := match fs.changes with
    | #[] => ""
    | _ => "Pending changes:\n" ++ (String.intercalate "\n" $ fs.changes.toList.map toString)
    s!"Recorded snapshots:\n{ssnaps}\n\n\
       {schanges}"

/-- Log entry tracking error. -/
structure TrackingError where
  message : String
  -- entry : LogEntry

instance : ToString TrackingError where
  toString te := te.message


/-- Global tracker. -/
structure Tracker where
  files : Std.TreeMap Uri FileState Ord.compare := {}
  errors : Array TrackingError := #[]
  line : Nat := 1
  requests : Std.TreeMap RequestID (Request Lean.Json) := {}
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
    s!"{fss}\n\nErrors:\n{errors}\n\nPending requests:\n{requests}"

/-- Tracker monad. -/
abbrev TrackerM := StateT Tracker IO
