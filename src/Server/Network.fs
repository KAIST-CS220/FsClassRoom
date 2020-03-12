// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.Network

open System.Net
open System.Net.Sockets

let getMyIP () =
  use socket =
    new Socket (AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp)
  socket.Connect ("8.8.8.8", 65530) // Open an arbitrary connection.
  let endPoint = socket.LocalEndPoint :?> IPEndPoint
  endPoint.Address.ToString ()
