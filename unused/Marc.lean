import Lean
import Std

section

open Lean
open Std
open JsonRpc
open Server.Test.Runner

instance : ToJson Time.ZonedDateTime where
  toJson dt := dt.toISO8601String

instance : FromJson Time.ZonedDateTime where
  fromJson?
    | .str s => Time.ZonedDateTime.fromISO8601String s
    | _ => throw "Expected string when converting JSON to Time.ZonedDateTime"

structure LogEntry where
  time : Time.ZonedDateTime
  direction : MessageDirection
  kind : MessageKind
  msg : JsonRpc.Message
  deriving FromJson, ToJson

namespace Lean.Server.Test.Runner.Client

def InteractiveGoalCore.pretty (g : InteractiveGoalCore) (userName? : Option String)
    (goalPrefix : String) : Format := Id.run do
  let indent := 2 -- Use option
  let mut ret := match userName? with
    | some userName => f!"case {userName}"
    | none          => Format.nil
  for hyp in g.hyps do
    ret := addLine ret
    let names := hyp.names
        |>.toList
        |>.filter (· != toString Name.anonymous)
        |> " ".intercalate
    match names with
    | "" =>
      ret := ret ++ Format.group f!":{Format.nest indent (Format.line ++ hyp.type.stripTags)}"
    | _ =>
      match hyp.val? with
      | some val =>
        ret := ret ++ Format.group f!"{names} : {hyp.type.stripTags} :={Format.nest indent (Format.line ++ val.stripTags)}"
      | none =>
        ret := ret ++ Format.group f!"{names} :{Format.nest indent (Format.line ++ hyp.type.stripTags)}"
  ret := addLine ret
  ret ++ f!"{goalPrefix}{Format.nest indent g.type.stripTags}"
where
  addLine (fmt : Format) : Format :=
    if fmt.isNil then fmt else fmt ++ Format.line

def InteractiveGoal.pretty (g : InteractiveGoal) : Format :=
  g.toInteractiveGoalCore.pretty g.userName? g.goalPrefix

end Lean.Server.Test.Runner.Client

def f (path : System.FilePath): IO Unit := do
  let log ← IO.FS.readFile path
  let log := log.trimRight
  let entries := log.splitOn "\n" |>.toArray
  let entries := entries.map Json.parse
  let entries ← IO.ofExcept <| entries.mapM id
  let entries : Array LogEntry ← IO.ofExcept <| entries.mapM fromJson?
  let getInteractiveGoalsIds := entries.filterMap fun e => do
    let req ← Request.ofMessage? e.msg
    guard <| req.method = "$/lean/rpc/call"
    let .ok (p : Lsp.RpcCallParams) := fromJson? req.param
      | none
    guard <| p.method = `Lean.Widget.getInteractiveGoals
    return req.id
  let getInteractiveGoalsIdIndex := HashSet.ofArray getInteractiveGoalsIds
  let getInteractiveGoalsResponses := entries.filterMap fun e => do
    let resp ← Response.ofMessage? e.msg
    guard <| getInteractiveGoalsIdIndex.contains resp.id
    let .ok (p : Client.InteractiveGoals) := fromJson? resp.result
      | none
    let goals := p.goals.map (·.pretty)
    return goals.map toString |>.toList |> "\n\n".intercalate
  let fmt := getInteractiveGoalsResponses.toList |> "\n\n-----\n\n".intercalate
  IO.println fmt

#eval f (System.mkFilePath [".", "logs", "LSP_2025-11-25-16-54-08-9505+0100.log"])
