// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.Checker

open System.Reflection
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
    msgqueue.Enqueue (
      succeed.ToString () + "/" + total.ToString () + " completed.")
    String.concat "\n" msgqueue
  with msg ->
    "[Error] " + msg.Message + "\n\n\
     This means your file cannot be compiled, or\n\
     your function has a wrong signature (parameter types are wrong).\n\n"
    + "[Log]\n\n" + String.concat "\n" msgqueue + "\n"
