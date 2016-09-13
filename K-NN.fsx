#load "packages/FsLab/FsLab.fsx"
open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle
open XPlot.Plotly

let rgen = System.Random()

type Data<'C,'V> = 
    {Class: 'C; Value: 'V}

let inline knn<'C,'V when 'C: equality and 'C: comparison> (data: Data<'C,'V> []) (value: 'V) k (d: 'V -> 'V -> float) =
    data 
    |> Array.Parallel.map(fun data -> d value data.Value, data.Class)
    |> Array.sortBy fst
    |> Array.take k
    |> Array.groupBy snd
    |> Array.minBy fst
    |> fun (_, pair) -> {Class = snd pair.[0]; Value = value}

let set = [|for i in 1..100 -> {Class = i; Value = rgen.NextDouble()*(10.* float i)}|]

set 
|> Array.groupBy (fun d -> d.Class) 
|> Array.sortBy fst
|> Array.map (fun (k, v) -> k, v.Length)

let value = rgen.NextDouble()*100.

knn set value 5 (fun a b -> abs (a - b) )


type KdTree<'K, 'V> = 
    | Node of key: 'K * value: 'V * left: KdTree<'K,'V> * right: KdTree<'K,'V>
    | Leaf of values: 'V list
    | NullLeaf

