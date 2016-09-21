#load "packages/FsLab/FsLab.fsx"
open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle
open XPlot.Plotly
open System.Collections.Generic

let rgen = System.Random()

type Data<'C,'V> = 
    {Class: 'C; Value: 'V}

let inline knn<'C,'V when 'C: equality and 'C: comparison> (data: Data<'C,'V> []) (value: 'V) k (d: 'V -> 'V -> float) =
    let take count array =
        if array |> Array.length < count then array else array |> Array.take count
    data 
    |> Array.Parallel.map(fun data -> d value data.Value, data.Class)
    |> Array.sortBy fst
    |> take k
    |> Array.groupBy snd
    |> Array.minBy fst
    |> fun (_, pair) -> {Class = snd pair.[0]; Value = value}

type KdTree<'K, 'V> = 
    | Node of left: KdTree<'K,'V> * key: 'K * values: 'V [] * right: KdTree<'K,'V>
    | Null

let createTree key value = 
    Node(Null, key, value,Null)

let add key value tree = 
    let rec innerAdd head =
        match head with
        | Node(left, nodeKey, values, right)   -> 
            if key = nodeKey then 
                Node(left, nodeKey, values |> Array.append [|value|], right) 
            else if nodeKey < key then 
                Node(left, nodeKey, values, innerAdd right) 
            else 
                Node(innerAdd left, nodeKey, values, right)
        | Null          -> Node(Null, key, [|value|], Null)
    innerAdd tree

let getMin tree = 
    let rec inner min = function 
        | Node(l, k, v, _)  -> inner (if k < fst min then k, v else min) l
        | Null              -> min
    let min = function | Node(_,k,v,_) -> k,v | _ -> failwith "Tree is empty!"
    inner (min tree) tree


let inline createKdTree<'C,'V when 'C: equality and 'C: comparison> (data: Data<'C,'V> []) (value: 'V) (d: 'V -> 'V -> float) =
    data 
    |> Array.fold (fun state data -> let key = d value data.Value in add key (data.Value, data.Class) state) Null 

let inline getNN first value tree d e = 
    let d = d first value
    let rec inner result = function 
        | Node(l, k, v, _)  -> inner (if abs(k - d) < e then v::result else result) l
        | Null              -> result
    inner [] tree

type Point = {X: float; Y: float}

let set = [|for i in 1..1000000 -> {Class = i % 10; Value = {X = rgen.NextDouble()*100.; Y = rgen.NextDouble()*100.}}|]
let value = {X = rgen.NextDouble()*100.; Y = rgen.NextDouble()*100.}
let tree = createKdTree set {X = 0.; Y = 0.} (fun a b -> sqrt(pown (a.X - b.X) 2 + pown (a.Y - b.Y) 2))

#time "on"
knn set value 5 (fun a b -> sqrt(pown (a.X - b.X) 2 + pown (a.Y - b.Y) 2))
#time "off"
#time "on"
getNN {X = 0.; Y = 0.} value tree (fun a b -> sqrt(pown (a.X - b.X) 2 + pown (a.Y - b.Y) 2)) 2.
#time "off"

