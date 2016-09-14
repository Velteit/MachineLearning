#load "packages/FsLab/FsLab.fsx"
#load "Utils.Math.fs"
#load "Utils.CSV.fs"
#load "Utils.Misc.fs"

open Deedle

open FSharp.Data

open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle
open XPlot.Plotly

open System
open System.IO

open Utils.Math
open Utils.CSV
open Utils.Misc

let rgen = Random(10201)
let datasetPath = Path.Combine(__SOURCE_DIRECTORY__, "datasets", "iris", "iris.csv")

type IrisType =
    | IrisSetosa
    | IrisVirginica
    | IrisVersicolor
    override this.ToString() =
        match this with
        | IrisSetosa  -> "Iris-setosa"
        | IrisVirginica -> "Iris-virginica"
        | IrisVersicolor -> "Iris-versicolor"
    static member Parse (str: string) =
        match str.ToLower().Replace("-","") with
        | "irissetosa" -> IrisSetosa
        | "irisvirginica" -> IrisVirginica
        | "irisversicolor" -> IrisVersicolor
        | a -> failwith ("Unknown type: " + a)


type Iris =
    {Type : IrisType; SepalLengthCm: float; SepalWidthCm: float; PetalLengthCm: float; PetalWidthCm: float;}

type Centroid<'T> = 
    {Center: 'T; Elements: 'T []}

let createCluster elements mean = 
    {Center = mean elements; Elements = elements}

let dataset =
    readCSV datasetPath ',' (fun text ->
            {   SepalLengthCm = float text.[1]
                SepalWidthCm = float text.[2]
                PetalLengthCm = float text.[3]
                PetalWidthCm = float text.[4]
                Type = text.[5]  |> IrisType.Parse })
    
let (setosa,virginica,versicolor)= 
    dataset 
    |> Array.groupBy (fun iris -> iris.Type)
    |> fun grouped -> grouped |> Array.find (fst >> ((=) IrisType.IrisSetosa)) |> snd, 
                      grouped |> Array.find (fst >> ((=) IrisType.IrisVirginica)) |> snd, 
                      grouped |> Array.find (fst >> ((=) IrisType.IrisVersicolor)) |> snd

let trainingSetosa = setosa |> Array.take 30
let trainingViriginica = virginica |> Array.take 30
let trainingVersicolor = versicolor |> Array.take 30

let data = Array.concat [setosa |> Array.skip 30;  virginica |> Array.skip 30; versicolor |> Array.skip 30] |> Array.shuffle


let irisMean t (el : Iris []) = 
    let count = el |> Array.length |> float in
        el |> Array.fold (fun (sl,sw,pl,pw) i -> sl + i.SepalLengthCm, sw + i.SepalWidthCm, pl + i.PetalLengthCm, pw + i.PetalWidthCm) (0.,0.,0.,0. )
        |> fun (sl,sw,pl,pw) -> { Type = t; SepalLengthCm = sl/count; SepalWidthCm = sw/count; PetalLengthCm = pl/count; PetalWidthCm = pw/count}

let d (i1: Iris) (i2: Iris) = 
    sqrt (((i1.SepalWidthCm - i2.SepalWidthCm)^2) + ((i1.SepalLengthCm - i2.SepalLengthCm)^2) + ((i1.PetalLengthCm - i2.PetalLengthCm)^2) + ((i1.PetalWidthCm - i2.PetalWidthCm)^2))

let predictType ts (clusters: Centroid<Iris> []) = 
    clusters 
    |> Array.minBy(fun c -> d ts c.Center)
    |> fun i -> i.Center.Type

let clusters = 
    [|  createCluster trainingSetosa (irisMean IrisSetosa); 
        createCluster trainingViriginica (irisMean IrisVirginica);
        createCluster trainingVersicolor (irisMean IrisVersicolor)|] 
clusters |> Array.map(fun c -> c.Center) 


[for ts in data -> predictType ts clusters = ts.Type, predictType ts clusters, ts] |> List.filter (fun (f,_,_) -> not f)