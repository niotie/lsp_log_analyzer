import LSPLogAnalyzer.Basic

namespace LSPLogAnalyzer

open System

def dumpFileStates (path : FilePath) : IO Unit := do
  let .some "log" := path.extension
    | IO.eprint s!"path should have extension .log"
  let .some stem := path.fileStem
    | IO.eprint s!"could not determine path stem"
  let .some dir := path.parent
    | IO.eprint s!"could not determine parent dir"
  let dirPath := dir.join (System.mkFilePath [stem])
  IO.FS.createDirAll dirPath
  let (_, st) ← processLogFile path |>.run {}
  st.files.forM (
    fun (uri : Uri) (fs : FileState) => do
      fs.snapshots.forM (
        fun snap => do
          let .some fileStem := System.FilePath.fileStem uri
            | IO.eprint s!"could not determine file name for {uri}"
          let fileName := s!"{fileStem}.{snap.time.format "yyyy-MM-dd.HH:mm:ss"}.lean"
          let path' := dirPath.join fileName
          let stream := IO.FS.Stream.ofHandle (← IO.FS.Handle.mk path' IO.FS.Mode.write)
          IO.FS.Stream.putStr stream snap.doc.text
      )
      -- let fpath := dir.join uri |>.join
  )

end LSPLogAnalyzer
