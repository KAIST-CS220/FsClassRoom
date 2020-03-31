// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

open System.Reflection
open System.IO
open System.IO.Pipes
open System.Collections.Generic

let check (asm: Assembly) method =
  let mutable total = 0
  let mutable succeed = 0
  let msgqueue = Queue<string> ()
  try
    for t in asm.GetTypes () do
      for m in t.GetMethods () do
        if m.Name.StartsWith "test" then
          total <- total + 1
          msgqueue.Enqueue ("[" + total.ToString () + "] testing " + m.Name)
          if m.Invoke (null, [| method |]) |> unbox<bool> then
            msgqueue.Enqueue ("Success!")
            succeed <- succeed + 1
          else
            msgqueue.Enqueue ("Failed.")
        else ()
    let score = float succeed / float total
    msgqueue.Enqueue (
      succeed.ToString () + "/" + total.ToString () + " completed.")
    score, String.concat "\n" msgqueue
  with msg ->
    0.0,
    "[Error] " + msg.Message + "\n\n\
     This means your implementation ran well,\n\
     but we encountered timeout or a fatal error during the evaluation.\n\n"
    + "[Log]\n\n" + String.concat "\n" msgqueue + "\n"

[<EntryPoint>]
let main argv =
  use cli = new AnonymousPipeClientStream (PipeDirection.Out, argv.[0])
  use sw = new StreamWriter (cli)
  let myfuncDllPath = argv.[1]
  let moduleName = argv.[2]
  let activityName = argv.[3]
  let testDllPath = argv.[4]
  let libDir = Path.GetDirectoryName testDllPath
  let libDllPath = Path.Combine (libDir, "Lib.dll")
  let _lib = Assembly.LoadFile (libDllPath)
  let test = Assembly.LoadFile (testDllPath)
  System.AppDomain.CurrentDomain.add_AssemblyResolve (
    System.ResolveEventHandler (fun _ args ->
      let asm =
        System.AppDomain.CurrentDomain.GetAssemblies ()
        |> Array.tryFind (fun loaded ->
          args.Name = loaded.FullName
          || args.Name = loaded.GetName().Name)
      defaultArg asm null))
  let myfunc = Assembly.LoadFile (myfuncDllPath)
  async { do! Async.Sleep (3000) (* Maximum 3 sec. *)
          do System.Environment.Exit(1) } |> Async.Start
  let score, msg =
    match myfunc.GetTypes () |> Array.tryFind (fun t -> t.Name = moduleName) with
    | None -> 0.0, "Module not found (typo in your module?)."
    | Some t ->
      if t.Namespace <> activityName then
        0.0, "Invalid module namespace (typo in your module?)."
      else
        match t.GetMethods () |> Array.tryFind (fun m -> m.Name = "myfunc") with
        | None -> 0.0, "Function not found (typo in your function?)."
        | Some m -> check test m
  sw.WriteLine ("{0:F}", score)
  sw.Write (msg)
  0 // return an integer exit code
