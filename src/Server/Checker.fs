// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.Checker

open FsClassroom.DB
open System.Collections.Generic

let check (ctxt: DB.Context) student method =
  let mutable total = 0
  let mutable succeed = 0
  let msgqueue = Queue<string> ()
  let asm = ctxt.TestDll
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
    ctxt.Submissions.[student.SID] <- { Submitter = student; Score = score }
    msgqueue.Enqueue (
      succeed.ToString () + "/" + total.ToString () + " completed.")
    String.concat "\n" msgqueue
  with msg ->
    ctxt.Submissions.[student.SID] <- { Submitter = student; Score = 0.0 }
    "[Error] " + msg.Message + "\n\n\
     This means your file cannot be compiled, or\n\
     you have an invalid module name (maybe typo), or\n\
     your function has a wrong signature (parameter types are wrong).\n\n"
    + "[Log]\n\n" + String.concat "\n" msgqueue + "\n"
