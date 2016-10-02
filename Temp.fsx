#r @"packages/FSharp.Quotations.Evaluator/lib/net40/FSharp.Quotations.Evaluator.dll"

#load "Utils.Misc.fs"
#load "Trees.fs"

open Trees
open System.Runtime.Serialization.Formatters.Binary
open System.IO

open Utils.Misc

let rgen = System.Random()

let set = [for _ in 1..10 -> rgen.NextDouble() * 10.]
set |> List.distinct
let tree =  BinaryTree.create set (fun a b -> if a = b then 0 else if a > b then 1 else -1)

let formatter = BinaryFormatter()
let stream = new FileStream("test.bin", FileMode.Open, FileAccess.Read)
let q = formatter.Deserialize(stream) :?> Microsoft.FSharp.Quotations.Expr<int -> int>
stream.Close()
stream.Dispose()
System.Environment.OSVersion.Platform
(<@ fun x -> x + 1 @> |> Expr.toLambda<int -> int>) 1
