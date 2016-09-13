#load "packages/FsLab/FsLab.fsx"
#load "Utils.Math.fs"
#load "Utils.CSV.fs"

open Deedle

open FSharp.Data

open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle
open XPlot.Plotly

open System
open System.IO

open Utils.Math

open Utils.CSV

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

let dataset =
    readCSV datasetPath ',' (fun text ->
            {   SepalLengthCm = float text.[1]
                SepalWidthCm = float text.[2]
                PetalLengthCm = float text.[3]
                PetalWidthCm = float text.[4]
                Type = text.[5]  |> IrisType.Parse })


let distance (i1: Iris) (i2: Iris) =
    (Σ [|   (i1.SepalLengthCm - i2.SepalLengthCm)^4;
            (i1.SepalWidthCm - i2.SepalWidthCm)^4;
            (i1.PetalLengthCm - i2.PetalLengthCm)^4;
            (i1.PetalWidthCm - i2.PetalWidthCm)^4 |]) ^0.25
    
let grouped = 
    dataset 
    |> Array.groupBy (fun iris -> iris.Type)
    |> Array.map(fun (_,collection) -> collection |> Array.pairwise |> Array.Parallel.map (fun (i1,i2) -> distance i1 i2) )

[|1..10|] |> Array.pairwise
