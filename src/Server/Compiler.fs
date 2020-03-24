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
  let dllpath = Path.GetFileName submissionPath + ".dll"
  let opts = [| "fsc.exe"; "-o"; dllpath; "-r"; libPath; "-a"; submissionPath |]
  let err, _, asm =
    checker.CompileToDynamicAssembly (opts, None)
    |> Async.RunSynchronously
  match asm with
  | Some asm -> Ok asm
  | None ->
    err |> Array.fold (fun acc inf ->
      acc
      + "(" + inf.StartLineAlternate.ToString ()
      + "," + (inf.StartColumn + 1).ToString ()
      + ") " + inf.Message + "\n") "" |> Error

let compileSubmission libPath submissionPath =
  let checker = FSharpChecker.Create ()
  if Sanitizer.sanitize submissionPath checker then
    "Sanitization failed. The following identifiers are banned: "
    + (Sanitizer.unsafe |> String.concat ", ")
    + "\n" |> Error
  else run libPath submissionPath checker
