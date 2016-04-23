open System.Net

let sites = ["http://www.google.com"; "http://www.amazon.com"; "http://www.apple.com"]
let results =
  sites
  |> Seq.map (
       fun s -> async {
                  printfn "%s" s
                  let wc = new WebClient()
                  let! data = wc.AsyncDownloadString(new System.Uri(s))
                  return data.Length
                })
  |> Async.Parallel
  |> Async.RunSynchronously

