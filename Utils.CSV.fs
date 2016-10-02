module Utils.CSV

open System.IO

let readCSV path sepChr mapper =
  let sepChr = [|sepChr|]
  File.ReadAllLines path
  |> fun text -> text.[1..] |> Array.Parallel.map (fun text -> sepChr |> (text.Split >> mapper))

let writeCSV path sepChr mapper data
  use stream = FileStream(path)
  let write msg =

  data
  |> Seq.iter (mapper >> (String.concat sepchar) >> stream)
