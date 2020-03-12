open System
open System.IO
open System.Net.Http

let send url content =
  async {
    use client = new HttpClient (BaseAddress=new Uri(url))
    let! msg = client.PostAsync ("submit", content) |> Async.AwaitTask
    return! msg.Content.ReadAsStringAsync () |> Async.AwaitTask
  }

[<EntryPoint>]
let main argv =
  let url = argv.[0]
  let token = argv.[1]
  let sid = argv.[2]
  let lastname = argv.[3]
  let filepath = argv.[4]
  use stream = new StreamContent (new MemoryStream (File.ReadAllBytes filepath))
  use content = new MultipartFormDataContent ()
  stream.Headers.Add ("Content-Type", "application/octet-stream")
  stream.Headers.Add ("Content-Disposition",
    "form-data; name=\"code\"; filename=\"code.fs\"")
  content.Add (stream, "code")
  content.Add (new StringContent (sid), "sid")
  content.Add (new StringContent (lastname), "lastname")
  content.Add (new StringContent (token), "token")
  send url content
  |> Async.RunSynchronously
  |> fun s -> printfn "%s" s
  0
