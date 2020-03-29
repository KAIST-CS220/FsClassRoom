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
open System.Threading

let [<Literal>] myport = 8080

let index =
  let dir =
    Path.GetFullPath (Reflection.Assembly.GetExecutingAssembly().Location)
    |> Path.GetDirectoryName
  Path.Combine (dir, "index.html")
  |> File.ReadAllText

let processSubmission ctxt student tmppath =
  let sid = DB.getSID student
  let submission = DB.log ctxt sid tmppath
  let moduleName = "M" + sid
  match Compiler.compileSubmission ctxt.LibDllPath submission with
  | Error msg -> FORBIDDEN msg
  | Ok asm ->
    match asm.GetTypes () |> Array.tryFind (fun t -> t.Name = moduleName) with
    | None -> FORBIDDEN "Module not found (typo in your module?).\n"
    | Some t ->
      if t.Namespace <> ctxt.ActivityName then
        FORBIDDEN "Invalid module namespace (typo in your module?).\n"
      else
        match t.GetMethods () |> Array.tryFind (fun m -> m.Name = "myfunc") with
        | None -> FORBIDDEN "Function not found (typo in your function?).\n"
        | Some m -> OK (Checker.check ctxt student m)

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

let getContext libfile testfile sessionDir =
  match sessionDir with
  | Some sessionDir ->
    printfn "Reload DB from %s" sessionDir
    DB.reload libfile testfile sessionDir
  | None ->
    let startTime = DateTime.Now.ToString ("yyyy.MM.dd-H.mm.ss")
    DB.init startTime libfile testfile

[<EntryPoint>]
let main args =
  let libfile, testfile = args.[0], args.[1]
  let sessionDir = try Some args.[2] with _ -> None
  let ctxt = getContext libfile testfile sessionDir
  startService ctxt
  DB.close ctxt
  0
