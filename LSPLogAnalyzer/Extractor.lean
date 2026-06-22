import LSPLogAnalyzer
import Cli

open Cli

def myCommand (args : Cli.Parsed) : IO UInt32 := do
  return 0

def extract : Cmd := `[Cli|
  extract VIA myCommand; ["0.0.1"]
  "Export the InfoTrees for a file as JSON."

  FLAGS:
    "tactics";      "Only export TacticInfo nodes."
    "original";     "Skip nodes with synthetic syntax."
    "substantive";  "Skip tactic combinators."

  ARGS:
    module : ModuleName; "Lean module to compile and export InfoTrees."
]
