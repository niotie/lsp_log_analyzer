-- open Lean
-- open JsonRpc
-- open Json
-- open Lsp

-- #print Position
-- #print Range
-- #print TextDocumentItem
-- #print DidChangeTextDocumentParams.textDocument

-- def wanted_methods := ["textDocument/didOpen", "textDocument/didChange"]

-- structure Change where
--   range : Range
--   length : Int
--   text : String
--   deriving Inhabited, Repr

-- instance : FromJson Change where
--   fromJson? j := do
--     let range ← j.getObjValAs? Range "range"
--     let len ← j.getObjValAs? Int "rangeLength"
--     let text ← j.getObjValAs? String "text"
--     return { range := range, length := len, text := text }

-- structure TextDocumentDidOpen where
--   uri : String
--   text : String
--   deriving Inhabited, Repr

-- def parseDidOpen (j : Json) : Except String TextDocumentDidOpen := do
--   let textDocument ← j.getObjVal? "params" >>= (getObjVal? · "textDocument")
--   let uri ← textDocument.getObjValAs? String "uri"
--   let text ← textDocument.getObjValAs? String "text"
--   return { uri := uri, text := text }

-- instance : FromJson TextDocumentDidOpen where
--   fromJson? := parseDidOpen

-- structure TextDocumentDidChange where
--   uri : String
--   changes : List Change
--   deriving Inhabited, Repr

-- def parseDidChange (j : Json) : Except String TextDocumentDidChange := do
--   let params ← j.getObjVal? "params"
--   let uri ← params.getObjVal? "textDocument" >>= (getObjValAs? · String "uri")
--   let changes ← params.getObjValAs? (List Change) "contentChanges"
--   return { uri := uri, changes := changes }

-- instance : FromJson TextDocumentDidChange where
--   fromJson? := parseDidChange

-- instance : ToString JsonRpc.Message where
--   toString := fun m =>
--   repr m.method ++ repr m.params
--   _

-- def ex_change_string := "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didChange\",\"params\":{\"textDocument\":{\"uri\":\"file:///home/ameyer/Documents/Code/lean4/lean_log_cruncher/Example.lean\",\"version\":2},\"contentChanges\":[{\"range\":{\"start\":{\"line\":6,\"character\":0},\"end\":{\"line\":7,\"character\":0}},\"rangeLength\":11,\"text\":\"\"}]}}"
-- -- def ex_change : Except String TextDocumentContentChangeEvent := parse ex_change_string >>= fromJson?
-- def ex_change : Except String JsonRpc.Message := parse ex_change_string >>= fromJson?
-- #eval ex_change

-- inductive LogEntry
-- | didOpen (e : TextDocumentDidOpen)
-- | didChange (e : TextDocumentDidChange)
-- deriving Inhabited, Repr

-- instance : FromJson LogEntry where
--   fromJson? j := do
--     let method : String ← j.getObjValAs? String "method"
--     match method with
--     | "textDocument/didOpen" => do
--       let e ← parseDidOpen j
--       Except.ok (LogEntry.didOpen e)
--     | "textDocument/didChange" => do
--       let e ← parseDidChange j
--       Except.ok (LogEntry.didChange e)
--     | _ => Except.error "unknown method"

-- structure Log where
--   entries : Array LogEntry
-- deriving ToJson, FromJson, Inhabited, Repr

-- def test_log_string :=
  --  "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":{\"textDocument\":{\"uri\":\"file:///home/ameyer/Documents/Code/lean4/lean_log_cruncher/Example.lean\",\"languageId\":\"lean4\",\"version\":1,\"text\":\"variable {p q r : Prop}\\n\\n\"},\"dependencyBuildMode\":\"never\"}}Content-Length: 171"
  --  "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":{\"textDocument\":{\"uri\":\"file:///home/ameyer/Documents/Code/lean4/lean_log_cruncher/Example.lean\",\"languageId\":\"lean4\",\"version\":1,\"text\":\"variable {p q r : Prop}\\n\\ntheorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by\\n  intro hp\\n  apply hqr  -- et pas hpq !\\n  apply hpq\\n  exact hp\\n\"},\"dependencyBuildMode\":\"never\"}}Content-Length: 171\n\n{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":{\"textDocument\":{\"uri\":\"file:///home/ameyer/Documents/Code/lean4/lean_log_cruncher/Example.lean\",\"languageId\":\"lean4\",\"version\":1,\"text\":\"variable {p q r : Prop}\\n\\ntheorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by\\n  intro hp\\n  apply hqr  -- et pas hpq !\\n  apply hpq\\n  exact hp\\n\"},\"dependencyBuildMode\":\"never\"}}Content-Length: 171"
--
-- def parse_logfile_line (line : String) : Except String LogEntry :=
  -- match line.trim.splitOn "Content-Length: " with
  -- | [""] => Except.error "empty line"
  -- | [s] | [s, _] => match Json.parse s >>= fromJson? with
    -- | Except.ok j => Except.ok j
    -- | e => e
  -- | _ => Except.error "unexpected line content"
--
-- #eval (test_log_string.splitOn "\n").map parse_logfile_line
