#r @"packages/FSharp.Quotations.Evaluator/lib/net40/FSharp.Quotations.Evaluator.dll"

#load "Utils.Misc.fs"
#load "Trees.fs"

open Trees
let rgen = System.Random()

let set = [for _ in 1..10 -> rgen.NextDouble() * 10.]
set |> List.distinct
let tree =  BinaryTree.create set (fun a b -> if a = b then 0 else if a > b then 1 else -1)