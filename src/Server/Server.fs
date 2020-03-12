// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.Server

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open System
open System.Threading

let [<Literal>] myport = 8080

let index = """
<html>
<head>
  <title>F# Classroom</title>
</head>
<body>
  <form action="submit" method="post" enctype="multipart/form-data">
    <label>
      Student ID:
      <input type="text" name="sid"/>
    </label>
    <br/>
    <label>
      Last Name:
      <input type="text" name="lastname"/>
    </label>
    <br/>
    <label>
      Token:
      <input type="text" name="token"/>
    </label>
    <br/>
    <label>
      <input type="file" name="code"/>
    </label>
    <br/>
    <input type="submit"/>
  </form>
</body>
</html>
"""

let processSubmission ctxt sid tmppath =
  let submission = DB.log ctxt sid tmppath
  let moduleName = "M" + sid
  match Compiler.compileSubmission ctxt.LibDllPath submission with
  | Error msg -> FORBIDDEN msg
  | Ok asm ->
    match asm.GetTypes () |> Array.tryFind (fun t -> t.Name = moduleName) with
    | None -> FORBIDDEN "Module not found.\n"
    | Some t ->
      match t.GetMethods () |> Array.tryFind (fun m -> m.Name = "myfunc") with
      | None -> FORBIDDEN "Function not found.\n"
      | Some m -> OK (Checker.check ctxt.TestDll m)

let handleSubmission ctxt sid lastname token tmppath =
  match DB.findStudent ctxt sid with
  | None -> never
  | Some s ->
    if s.LastName = lastname (* && token = DB.getToken ctxt *) then
      processSubmission ctxt sid tmppath
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
  Console.Write ("{0} > ",
    (if cts.IsCancellationRequested then "(no connection)" else ""))
  match Console.ReadLine () |> String.split ' ' with
  | "stop" :: _ -> cts.Cancel (); prompt ctxt cts
  | "quit" :: _
  | "q" :: _ -> ()
  | "token" :: _ ->
    Console.WriteLine ("{0}", DB.getToken ctxt)
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

[<EntryPoint>]
let main args =
  let libfile, testfile = args.[0], args.[1]
  let startTime = DateTime.Now.ToString ("yyyy.MM.dd-H.mm.ss")
  let ctxt = DB.init startTime libfile testfile
  startService ctxt
  0
