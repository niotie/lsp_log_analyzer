import LSPLogAnalyzer

open LSPLogAnalyzer

def main (args : List String) : IO UInt32 := do
  match args with
  | [filename] =>
    let (_, st) ← processLogFile filename |>.run {}
    IO.println st
    return 0
  | _ => return 1
