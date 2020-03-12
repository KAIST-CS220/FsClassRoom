// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.Checker

open System.Reflection

let check (asm: Assembly) method =
  let mutable total = 0
  let mutable succeed = 0
  try
    for t in asm.GetTypes () do
      for m in t.GetMethods () do
        if m.Name.StartsWith "test" then
          total <- total + 1
          if m.Invoke (null, [| method |]) |> unbox<bool> then
            succeed <- succeed + 1
          else ()
        else ()
    sprintf "%d / %d complete.\n" succeed total
  with msg -> msg.Message + "\n"
