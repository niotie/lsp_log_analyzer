import LSPLogAnalyzer
import Lean.Parser.Command

open LSPLogAnalyzer
open Lean
open ToJson
open FromJson
open JsonRpc
open Lsp
open Std
open Server.Test.Runner

def test1 := show _ from do
  let path := System.mkFilePath [".", "logs",
    "LSP_2025-12-09-18-26-16-7165+0100.log"
  ]
  let (_, st) ← processLogFile path |>.run {}
  return st

-- Logs collectés par Clara dans le cours de Patrick
def test2 := show _ from do
  let path := System.mkFilePath [".", "logs", "patrick",
    "LSP_2026-02-11-15-37-46-3175+0100.log"
    -- LSP_2026-02-11-15-39-21-2234+0100.log
  ]
  let (_, st) ← processLogFile path |>.run {}
  return st

def test3 := show _ from do
  let path := System.mkFilePath [".", "logs", "patrick",
    "LSP_2026-02-11-15-37-46-3175+0100.log"
    -- LSP_2026-02-11-15-39-21-2234+0100.log
  ]
  dumpFileStates path

#eval test1
-- #eval test2
-- #eval test3

-- Test for InteractiveGoals
def f (path : System.FilePath): IO Unit := do
  let entries ← collectLogEntries path
  let getInteractiveGoalsIds := entries.filterMap fun e => do
    let req ← Request.ofMessage? e.msg
    guard <| req.method = "$/lean/rpc/call"
    let .ok (p : RpcCallParams) := fromJson? req.param
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

-- #eval f (System.mkFilePath [".", "logs", "LSP_2025-11-25-16-54-08-9505+0100.log"])


-- -- Test for the collection of definitions
def testCollectDefs fname contents := do
  let env ← prepareBaseEnv fname `DummyModule
  -- IO.println $ env.constants.toList.map (·.fst)
  let doc : TextDocumentItem := {
    uri := fname
    text := contents
    languageId := "Lean"
    version := 1
  }
  let defs ← runCollectDefLikes env doc
  defs.forM (IO.println ·)
  -- defs.forM (fun d : Definition => IO.println d.defview.ref)

def ex_fname := "/home/ameyer/Nextcloud/Eiffel/Code/lean4/lsp_log_analyzer/Example.lean"

def ex_contents :=
"import Lean

variable {p q r : Prop}

theorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by
  intro hp
  exact hqr (hpq hp)

theorem or_comm' (h : p ∨ q) : q ∨ p := by
  rcases h with h | h
  right
  exact h
  left
  exact h
"

-- #eval IO.println $ normalizeText ex_contents
-- #eval testCollectDefs ex_fname ex_contents

def verbose_contents :=
"import Mdd154.Lib
setup_env

namespace m154
open Nat
/-
# Feuille 4bis : Plus de ∀ et ∃

## Première partie sur ordinateur
-/

Exercice \"01\"
  Données : (u : ℕ → ℝ)
  Hypothèses : (h : ∀ n : ℕ, ∀ m : ℕ, u m = u n)
  Conclusion : ∀ n : ℕ, u n = u 0
Démonstration :
  Soit n
  Comme ∀ n : ℕ, ∀ m : ℕ, u m = u n on conclut que u n = u 0
QED

Exercice \"02\"
  Données : (u : ℕ → ℝ)
  Hypothèses : (h : ∀ n : ℕ, u n = u 0)
  Conclusion : ∀ n : ℕ, ∀ m : ℕ, u m = u n
Démonstration :
  Soit n m
  Comme ∀ n : ℕ, u n = u 0 on conclut que u m = u n
QED

Exercice \"03\"
  Données : (u : ℕ → ℝ)
  Hypothèses : (h : ∃ y : ℝ, ∀ n : ℕ, u n = y)
  Conclusion : ∃ n : ℕ, ∀ m : ℕ, u m = u n
Démonstration :
  Comme ∃ y : ℝ, ∀ n : ℕ, u n = y on obtient y tel que ∀ n : ℕ, u n = y
  Montrons que 0 convient : ∀ m : ℕ, u m = u 0
  Soit m
  Comme ∀ n : ℕ, u n = y on obtient que u 0 = y
  Comme ∀ n : ℕ, u n = y on obtient que u m = y
  Comme u 0 = y et u m = y on conclut que u m = u 0
QED

/-
## Partie sur ordinateur

Sur papier, démontrer les deux implications suivantes concernant une
suite u.

04) `(∃ n : ℕ, ∀ m : ℕ, u m = u n) ⇒ (∃ y : ℝ, ∀ n : ℕ, u n = y)`

puis

05) `(∀ n : ℕ, ∀ m : ℕ, u m = u n) ⇒ (∃ n : ℕ, ∀ m : ℕ, u m = u n)`.


## 2ème partie sur ordinateur
-/

Exercice \"06\"
  Données : (u : ℕ → ℝ)
  Hypothèses : (h : ∀ n : ℕ, ∀ m : ℕ, u m = u n)
  Conclusion : ∃ n : ℕ, ∀ m : ℕ, u m = u n
Démonstration :
  Montrons que 3 convient
  Comme ∀ n : ℕ, ∀ m : ℕ, u m = u n on conclut que ∀ m : ℕ, u m = u 3
QED

Exercice \"07\"
  Données : (u : ℕ → ℝ)
  Hypothèses : (h : ∃ n : ℕ, ∀ m : ℕ, u m = u n)
  Conclusion : ∀ n : ℕ, ∀ m : ℕ, u m = u n
Démonstration :
  Soit n m
  Comme ∃ n : ℕ, ∀ m : ℕ, u m = u n on obtient a tel que ∀ m' : ℕ, u m' = u a
  Comme ∀ m' : ℕ, u m' = u a on obtient que u n = u a
  Comme ∀ m' : ℕ, u m' = u a on obtient que u m = u a
  Comme u n = u a et u m = u a on conclut que u m = u n
QED
"

-- #eval testCollectDefs "toto.lean" verbose_contents
