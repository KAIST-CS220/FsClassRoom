// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.DB

open System.IO
open System.Text.RegularExpressions
open System.Reflection
open System.Collections.Generic
open System.Runtime.Serialization
open System.Runtime.Serialization.Json

[<DataContract>]
type Student = {
  [<field: DataMember(Name = "sid")>]
  SID: string
  [<field: DataMember(Name = "lastname")>]
  LastName: string
}

[<DataContract>]
type Submission = {
  [<field: DataMember(Name = "submitter")>]
  Submitter: Student
  [<field: DataMember(Name = "score")>]
  Score: float
}

type Context = {
  Token: string
  SessionDir: string
  LibDllPath: string
  TestDllPath: string
  CheckerDllPath: string
  Students: Map<string, Student>
  Submissions: Dictionary<string, Submission>
  ActivityName: string
}

let private rxSID = new Regex ("^20[0-2][0-9]{5}$", RegexOptions.Compiled)
let private rxLastName = new Regex ("^[a-zA-Z]+$", RegexOptions.Compiled)

let [<Literal>] private dbdir = "db"
let [<Literal>] private studentfile = "students.csv"
let [<Literal>] private studentdb = "students.db"
let [<Literal>] private submissiondb = "submission.db"
let [<Literal>] private resultsfile = "results.csv"

let private parseLine (line: string) =
  let arr = line.Split(',')
  arr.[0].Split(' ') |> Seq.last |> (fun s -> s.ToLower ()),
  arr.[1]

let private mkStudent (lastname : string, sid : string) =
  let mutable errorMsg = ""
  let ckName = (if rxLastName.IsMatch lastname then Some lastname
    else errorMsg <- errorMsg + sprintf "invalid name: %s. " lastname; None)
  let ckSID = (if rxSID.IsMatch sid then Some sid
    else errorMsg <- errorMsg + sprintf "invalid ID: %s. " sid; None)
  Option.map2 (fun sid lastname -> sid, { SID = sid; LastName = lastname }) ckName ckSID
    |> Option.orElse (printf "! Skipping student [%s] due to %s\n" sid errorMsg; None)

let private initDBDir () =
  if Directory.Exists dbdir then ()
  else Directory.CreateDirectory dbdir |> ignore

let private initSessionDir stime =
  Path.Combine (dbdir, stime)
  |> Directory.CreateDirectory
  |> fun info -> info.FullName

let private initStudents () =
  let dir = Path.Combine (dbdir, studentfile)
  if File.Exists dir |> not then
    failwith "Cannot load: students.csv doesn't exist."
  dir |> File.ReadLines
  |> Seq.fold (fun m line ->
               match parseLine line |> mkStudent with
                | Some (sid, student) -> Map.add sid student m
                | None -> 
                  m) Map.empty

let init stime libfile testfile checker =
  initDBDir ()
  let libpath, testpath = Compiler.compileTest libfile testfile
  let testDll = Assembly.LoadFile testpath
  { Token = Random.str 10
    SessionDir = initSessionDir stime
    LibDllPath = libpath
    TestDllPath = testpath
    CheckerDllPath = checker
    Students = initStudents ()
    Submissions = Dictionary ()
    ActivityName = testDll.GetTypes().[0].Namespace }

let assertDBExistence dbpath =
  if File.Exists dbpath then ()
  else failwith "Cannot reload: DB doesn't exist."

let deserialize<'T> path =
  use fs = File.OpenRead path
  (new DataContractJsonSerializer(typeof<'T>)).ReadObject (fs) :?> 'T

let deserializeStudents sessionDir =
  let dbpath = Path.Combine (sessionDir, studentdb)
  assertDBExistence dbpath
  deserialize<Student []> dbpath
  |> Array.fold (fun map s -> Map.add s.SID s map) Map.empty

let deserializeSubmissions sessionDir =
  let dbpath = Path.Combine (sessionDir, submissiondb)
  let dict = Dictionary<string, Submission> ()
  assertDBExistence dbpath
  deserialize<Submission []> dbpath
  |> Array.iter (fun s -> dict.[s.Submitter.SID] <- s)
  dict

let reload libfile testfile checker sessionDir =
  Path.Combine (sessionDir, submissiondb) |> assertDBExistence
  let libpath, testpath = Compiler.compileTest libfile testfile
  let testDll = Assembly.LoadFile testpath
  { Token = Random.str 10
    SessionDir = sessionDir
    LibDllPath = libpath
    TestDllPath = testpath
    CheckerDllPath = checker
    Students = deserializeStudents sessionDir
    Submissions = deserializeSubmissions sessionDir
    ActivityName = testDll.GetTypes().[0].Namespace }

let getToken ctxt = ctxt.Token

let getSID student = student.SID

let findStudent ctxt sid = Map.tryFind sid ctxt.Students

let log ctxt sid tmpPath =
  let pattern = sid + ".????.fs"
  Directory.GetFiles (ctxt.SessionDir, pattern, SearchOption.TopDirectoryOnly)
  |> Array.fold (fun maxcnt f ->
    let tokens = f.Split [| '.' |]
    let cnt = tokens.[tokens.Length - 2] |> int
    max cnt maxcnt) 0
  |> fun cnt ->
    let nextcnt = cnt + 1
    let nextFile = sid + "." + nextcnt.ToString ("D4") + ".fs"
    let newPath = Path.Combine (ctxt.SessionDir, nextFile)
    File.Move (tmpPath, newPath)
    newPath

let json<'T> (obj: 'T) =
  use ms = new MemoryStream ()
  (new DataContractJsonSerializer(typeof<'T>)).WriteObject(ms, obj)
  ms.ToArray ()

let writeStudentDB ctxt =
  let dbpath = Path.Combine (ctxt.SessionDir, studentdb)
  ctxt.Students
  |> Seq.map (fun (KeyValue (_, student)) -> student)
  |> Seq.toArray
  |> json<Student []>
  |> fun bs -> File.WriteAllBytes (dbpath, bs)

let writeSubmissionDB ctxt =
  let submissionpath = Path.Combine (ctxt.SessionDir, submissiondb)
  ctxt.Submissions
  |> Seq.map (fun (KeyValue (_, submission)) -> submission)
  |> Seq.toArray
  |> json<Submission []>
  |> fun bs -> File.WriteAllBytes (submissionpath, bs)

let writeResults ctxt =
  let path = Path.Combine (ctxt.SessionDir, resultsfile)
  use f = File.CreateText (path)
  ctxt.Submissions
  |> Seq.iter (fun (KeyValue (_, submission)) ->
    let line = submission.Submitter.SID
    let line = line + ","
    let line = line + submission.Score.ToString ("0.00")
    f.WriteLine (line))

let close ctxt =
  writeStudentDB ctxt
  writeSubmissionDB ctxt
  writeResults ctxt
