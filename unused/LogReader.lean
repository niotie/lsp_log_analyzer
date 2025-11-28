import Lean.Data.JsonRpc
import Lean.Server.Logging

local instance : Lean.ToJson Std.Time.ZonedDateTime where
  toJson dt := dt.toISO8601String

local instance : Lean.FromJson Std.Time.ZonedDateTime where
  fromJson?
    | .str s => Std.Time.ZonedDateTime.fromISO8601String s
    | _ => throw "Expected string when converting JSON to Std.Time.ZonedDateTime"

structure LogEntry where
  time : Std.Time.ZonedDateTime
  direction : Lean.JsonRpc.MessageDirection
  kind : Lean.JsonRpc.MessageKind
  msg : Lean.JsonRpc.Message
  deriving Lean.FromJson, Lean.ToJson

def parseLogLine (line : String) : Except String LogEntry := do
  let j ← Lean.Json.parse line
  Lean.fromJson? j

instance : ToString LogEntry where
  toString le := Lean.Json.pretty $ Lean.ToJson.toJson le

#eval
  parseLogLine "{\"direction\":\"serverToClient\",\"kind\":\"response\",\"msg\":{\"id\":5,\"jsonrpc\":\"2.0\",\"result\":[]},\"time\":\"2025-10-30T11:38:44+01:00\"}"
