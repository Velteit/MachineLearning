module Utils.CSV

open System.IO

let readCSV path sepChr mapper =
    let sepChr = [|sepChr|]
    File.ReadAllLines path
    |> fun text -> text.[1..] |> Array.Parallel.map (fun text -> sepChr |> (text.Split >> mapper))
