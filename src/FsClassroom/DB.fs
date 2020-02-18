// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.DB

open System.IO
open System.Reflection

type Student = {
  SID: string
  LastName: string
}

type Submission = {
  Submitter: Student
  Code: string
}

type Context = {
  Token: string
  SessionDir: string
  LibDllPath: string
  TestDll: Assembly
  Students: Map<string, Student>
  Submissions: Map<string, Submission list>
}

let [<Literal>] private dbdir = "db"
let [<Literal>] private studentfile = "students.csv"

let private parseLine (line: string) =
  let arr = line.Split(',')
  arr.[0].Split(' ') |> Seq.last |> (fun s -> s.ToLower ()),
  arr.[1]

let private initDBDir () =
  if Directory.Exists dbdir then ()
  else Directory.CreateDirectory dbdir |> ignore

let private initSessionDir stime =
  Path.Combine (dbdir, stime)
  |> Directory.CreateDirectory
  |> fun info -> info.FullName

let private initStudents () =
  Path.Combine (dbdir, studentfile)
  |> File.ReadLines
  |> Seq.fold (fun m line ->
               let lastname, sid = parseLine line
               Map.add sid { SID = sid; LastName = lastname } m) Map.empty

let init stime libfile testfile =
  initDBDir ()
  let libpath, testpath = Compiler.compileTest libfile testfile
  { Token = Random.str 10
    SessionDir = initSessionDir stime
    LibDllPath = libpath
    TestDll = Assembly.LoadFile testpath
    Students = initStudents ()
    Submissions = Map.empty }

let getToken ctxt = ctxt.Token

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
