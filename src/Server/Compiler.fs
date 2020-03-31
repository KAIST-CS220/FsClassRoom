// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.Compiler

open System.IO
open FSharp.Compiler.SourceCodeServices

let compileTest libFilePath testFilePath =
  let checker = FSharpChecker.Create ()
  let libPath = Path.GetFullPath libFilePath
  let outLibPath = Path.ChangeExtension (libPath, "dll")
  let cmdopt = [| "fsc.exe"; "-o"; outLibPath; "-a"; libPath |]
  let info, _ = checker.Compile (cmdopt) |> Async.RunSynchronously
  if info.Length = 0 then ()
  else printfn "%A" info; failwith "Failed to compile lib file."
  let testPath = Path.GetFullPath testFilePath
  let outtestpath = Path.ChangeExtension (testPath, "dll")
  let cmdopt =
    [| "fsc.exe"; "-o"; outtestpath; "-r"; outLibPath; "-a"; testPath |]
  let info, _ = checker.Compile (cmdopt) |> Async.RunSynchronously
  if info.Length = 0 then outLibPath, outtestpath
  else printfn "%A" info; failwith "Failed to compile test file."

let run libPath (submissionPath: string) (checker: FSharpChecker) =
  let dir = Path.GetDirectoryName submissionPath
  let filename = Path.ChangeExtension (Path.GetFileName submissionPath, "dll")
  let dllpath = Path.Combine (dir, filename)
  let opts = [| "fsc.exe"; "-o"; dllpath; "-r"; libPath; "-a"; submissionPath |]
  let info, _ = checker.Compile (opts) |> Async.RunSynchronously
  if info.Length = 0 then Ok dllpath
  else
    info
    |> Array.fold (fun s i ->
      s
      + "Error: "
      + "(" + i.StartLineAlternate.ToString () + "," + i.StartColumn.ToString () + ") "
      + i.Message + "\n") ""
    |> Error

let compileSubmission libPath submissionPath =
  let checker = FSharpChecker.Create ()
  if Sanitizer.sanitize submissionPath checker then
    "Sanitization failed. The following identifiers are banned: "
    + (Sanitizer.unsafe |> String.concat ", ")
    + "\n" |> Error
  else run libPath submissionPath checker
