// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.Random

let str n =
  let chars = "abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNOPQRSTUVWUXYZ2345679"
  let len = String.length chars
  let r = new System.Random ()
  System.String (Array.init n (fun _ -> chars.[r.Next(len)]))
