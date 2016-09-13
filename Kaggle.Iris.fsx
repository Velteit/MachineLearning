#load "packages/FsLab/FsLab.fsx"
#r "packages/Google.DataTable.Net.Wrapper\lib\Google.DataTable.Net.Wrapper.dll"
open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle
open XPlot.Plotly
open System
open System.IO

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
        
let inline (^) a b = Math.Pow(float a, float b)

let readCSV path sepChr mapper = 
    let sepChr = [|sepChr|]
    File.ReadAllLines path
    |> fun text -> text.[1..]
    |> Array.Parallel.map (fun text -> sepChr |> (text.Split >> mapper))

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
    [   (i1.SepalLengthCm - i2.SepalLengthCm)^2
        (i1.SepalWidthCm - i2.SepalWidthCm)^2  
        (i1.PetalLengthCm - i2.PetalLengthCm)^2
        (i1.PetalWidthCm - i2.PetalWidthCm)^2 ]
    |> List.sum
    |> fun result -> result^0.5


    