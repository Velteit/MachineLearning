#r @"packages/FSharp.Quotations.Evaluator/lib/net40/FSharp.Quotations.Evaluator.dll"

#load "packages/FsLab/FsLab.fsx"
#load "Utils.Math.fs"
#load "Utils.CSV.fs"
#load "Utils.Misc.fs"
#load "Neural.fs"

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

open System.IO
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Providers.LinearAlgebra.Mkl

Control.NativeProviderPath <- __SOURCE_DIRECTORY__ +  @"\packages\MathNet.Numerics.MKL.Win-x64\build\x64"
Control.UseNativeMKL()

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
    member this.ToVector() = 
        [|this.SepalLengthCm; this.SepalWidthCm; this.PetalLengthCm; this.PetalWidthCm|] |> DenseVector.ofArray

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
let trainingVirginica = virginica |> Array.take 30
let trainingVersicolor = versicolor |> Array.take 30

let trainingData = Array.concat [|trainingSetosa; trainingVersicolor; trainingVirginica|]

let data = Array.concat [setosa |> Array.skip 30;  virginica |> Array.skip 30; versicolor |> Array.skip 30] |> Array.shuffle

let createNeuron alpha data itype =
    let weights = [rgen.NextDouble(); rgen.NextDouble(); rgen.NextDouble(); rgen.NextDouble()] |> DenseVector.ofList
    let expected = data |> Array.Parallel.map (fun iris -> if iris.Type = itype then 1. else -1.) |> DenseVector.ofArray
    let input = data |> Array.Parallel.map (fun iris -> iris.ToVector()) |> List.ofArray |> DenseMatrix.ofRows
    Neural.NeuralV2.create (weights) (rgen.NextDouble()) <@ fun x -> tanh x @> "x"
    |> Neural.NeuralV2.learn expected input (alpha, (fun yh y -> ((yh - y) |> Vector.map(fun r -> pown r 2) |> Vector.sum) * 0.5), 0.01) 

let setosaNeuron = 
    createNeuron 0.01 trainingData IrisSetosa 

data 
|> Array.Parallel.map(fun iris -> iris.Type, setosaNeuron |> Neural.NeuralV2.forward (iris.ToVector()))
|> Array.filter(fun (l,r) -> r > 0.)

let versicolorNeuron = 
    createNeuron 0.001 trainingData IrisVersicolor

let virginicaNeuron = 
    createNeuron 0.001 trainingData IrisVirginica


let v = [1.; 2.; 3.; 4.] |> DenseVector.ofList
v.PointwiseMultiply(v).DotProduct(v)