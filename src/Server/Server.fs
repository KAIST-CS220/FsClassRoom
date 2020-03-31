// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.Server

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open System
open System.IO
open System.IO.Pipes
open System.Threading
open System.Diagnostics

let [<Literal>] myport = 8080

let bindir =
  (Uri (Reflection.Assembly.GetExecutingAssembly().CodeBase)).AbsolutePath
  |> Path.GetDirectoryName

let index =
  Path.Combine (bindir, "index.html")
  |> File.ReadAllText

let forkChild (ctxt: DB.Context) student sid dllpath moduleName =
  use p = new Process ()
  use srv =
    new AnonymousPipeServerStream (PipeDirection.In,
                                   HandleInheritability.Inheritable)
  p.StartInfo.FileName <- "dotnet"
  p.StartInfo.Arguments <-
    ctxt.CheckerDllPath
    + " " + srv.GetClientHandleAsString ()
    + " \"" + dllpath + "\""
    + " \"" + moduleName + "\""
    + " \"" + ctxt.ActivityName + "\""
    + " \"" + ctxt.TestDllPath + "\""
  p.StartInfo.UseShellExecute <- false
  p.Start () |> ignore
  srv.DisposeLocalCopyOfClientHandle ()
  try
    use sr = new StreamReader (srv)
    p.WaitForExit ()
    p.Close ()
    let score = sr.ReadLine () |> float
    ctxt.Submissions.[sid] <- { Submitter = student; Score = score }
    sr.ReadToEnd ()
  with e ->
    "[Fatal Error] " + e.ToString ()

let processSubmission ctxt student tmppath =
  let sid = DB.getSID student
  let submission = DB.log ctxt sid tmppath
  let moduleName = "M" + sid
  match Compiler.compileSubmission ctxt.LibDllPath submission with
  | Error msg -> FORBIDDEN msg
  | Ok dllpath ->
    let res = forkChild ctxt student sid dllpath moduleName
    OK (res)

let handleSubmission ctxt sid lastname token tmppath =
  match DB.findStudent ctxt sid with
  | None -> never
  | Some s ->
    if s.LastName = lastname && token = DB.getToken ctxt then
      processSubmission ctxt s tmppath
    else never

let submit ctxt (req: HttpRequest) =
  cond (req.fieldData "sid") (fun sid ->
    cond (req.fieldData "lastname") (fun lastname ->
      cond (req.fieldData "token") (fun token ->
        let files = req.files
        let lastname = lastname.ToLower ()
        if files.Length = 0 then never
        else handleSubmission ctxt sid lastname token (files.[0].tempFilePath)
      ) never
    ) never
  ) never

let app ctxt =
  choose
    [ GET >=> choose [ path "/" >=> OK index ]
      POST >=> choose [ path "/submit" >=> request (submit ctxt) ]
      NOT_FOUND "Stay away! Don't play with the system.\n" ]

let getConfig ip port token =
  { defaultConfig with
      cancellationToken = token
      bindings = [ HttpBinding.createSimple HTTP ip port ]
      listenTimeout = TimeSpan.FromMilliseconds 3000. }

let rec prompt ctxt (cts: CancellationTokenSource) =
  let now = DateTime.Now
  Console.Write ("{0} {1} > ",
    (if cts.IsCancellationRequested then "(no connection)" else ""),
    now.ToString())
  match Console.ReadLine () |> String.split ' ' with
  | "stop" :: _ -> cts.Cancel (); prompt ctxt cts
  | "quit" :: _
  | "q" :: _ -> ()
  | "token" :: _ ->
    Console.WriteLine ("{0}", DB.getToken ctxt)
    prompt ctxt cts
  | "lower" :: score :: _ ->
    ctxt.Submissions
    |> Seq.filter (fun (KeyValue (_, s)) -> s.Score <= float score)
    |> Seq.iter (fun (KeyValue (_, s)) ->
      Console.WriteLine ("{0}: {1:F}", s.Submitter.SID, s.Score))
    prompt ctxt cts
  | "higher" :: score :: _ ->
    ctxt.Submissions
    |> Seq.filter (fun (KeyValue (_, s)) -> s.Score >= float score)
    |> Seq.iter (fun (KeyValue (_, s)) ->
      Console.WriteLine ("{0}: {1:F}", s.Submitter.SID, s.Score))
    prompt ctxt cts
  | "stat" :: _ ->
    let totalSubmissions = ctxt.Submissions.Count
    Console.WriteLine ("Total # of submissions: {0}", totalSubmissions)
    ctxt.Submissions
    |> Seq.countBy (fun (KeyValue (_, s)) -> s.Score)
    |> Seq.iter (fun (score, cnt) ->
      Console.WriteLine ("{0:F} => {1}", score, cnt))
    prompt ctxt cts
  | _ -> prompt ctxt cts

let startService ctxt =
  use cts = new CancellationTokenSource ()
  let myip = Network.getMyIP ()
  let cfg = getConfig myip myport cts.Token
  let _, server = startWebServerAsync cfg (app ctxt)
  Async.Start (server, cts.Token)
  Thread.Sleep (1000)
  prompt ctxt cts
  cts.Cancel ()
  Console.WriteLine ()

let getContext libfile testfile checker sessionDir =
  let checker =
    if File.Exists checker then Path.GetFullPath checker
    else failwith "Checker.dll should be given."
  match sessionDir with
  | Some sessionDir ->
    let sessionDir = Path.GetFullPath sessionDir
    printfn "Reload DB from %s" sessionDir
    DB.reload libfile testfile checker sessionDir
  | None ->
    let startTime = DateTime.Now.ToString ("yyyy.MM.dd-H.mm.ss")
    DB.init startTime libfile testfile checker

[<EntryPoint>]
let main args =
  let libfile, testfile, checker = args.[0], args.[1], args.[2]
  let sessionDir = try Some args.[3] with _ -> None
  let ctxt = getContext libfile testfile checker sessionDir
  startService ctxt
  DB.close ctxt
  0
