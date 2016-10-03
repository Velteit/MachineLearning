module Utils.CSV

open System.IO
open System.Text

let readCSV path sepChr mapper =
  let sepChr = [|sepChr|]
  File.ReadAllLines path
  |> fun text -> text.[1..] |> Array.Parallel.map (fun text -> sepChr |> (text.Split >> mapper))

let inline writeCSV (path : string) (sep: string) (headers: string seq) mapper data =
  use stream = new FileStream(path, FileMode.OpenOrCreate, FileAccess.Write)
  let write (msg: string) =
    let bytes = Encoding.UTF8.GetBytes msg 
    stream.Write(bytes, 0, bytes.Length)
  let eol () = 
    let bytes = Encoding.UTF8.GetBytes "\n" 
    stream.Write(bytes, 0, bytes.Length)
  headers |> ((String.concat sep) >> write >> eol)
  data
  |> Seq.iter (mapper >> (String.concat sep) >> write >> eol)
  stream.Flush(true)