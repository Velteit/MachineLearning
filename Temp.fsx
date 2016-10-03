#load "Opens.fsx"

open Trees
open System.Runtime.Serialization.Formatters.Binary
open System.IO

open Utils.CSV

let rgen = System.Random()

type Point = {X: float; Y: float}
let data = [for i in 1..10 -> {X = rgen.NextDouble(); Y = rgen.NextDouble()}]
let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "test.csv")
writeCSV path ";" ["X"; "Y"] (fun x -> [string x.X; string x.Y]) data