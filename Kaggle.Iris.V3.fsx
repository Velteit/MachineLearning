#load "Opens.fsx"

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

open Neural

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

let trainingData =
    Array.concat [|trainingSetosa; trainingVersicolor; trainingVirginica|]
    |> Array.shuffle

trainingData |> Array.map(fun iris -> iris.Type)
let data = dataset |> Array.shuffle

let hiddens = 
    [|1..3|] 
    |> Array.Parallel.map(fun _ -> 
        [|1..4|] |> Array.map(fun _ -> let weights = [1..4] |> List.map (fun _ -> rgen.NextDouble()) |> DenseVector.ofList in Neuron.create weights (rgen.NextDouble()) <@ fun x -> 1./(1. + exp (-2.*x)) @> "x"))

let outputs = 
  [|  Neuron.create ([1..4] |> List.map (fun _ -> rgen.NextDouble()) |> DenseVector.ofList) (rgen.NextDouble()) <@ fun x -> 1./(1. + exp (-2.*x)) @> "x"; 
      Neuron.create ([1..4] |> List.map (fun _ -> rgen.NextDouble()) |> DenseVector.ofList) (rgen.NextDouble()) <@ fun x -> 1./(1. + exp (-2.*x)) @> "x";
      Neuron.create ([1..4] |> List.map (fun _ -> rgen.NextDouble()) |> DenseVector.ofList) (rgen.NextDouble()) <@ fun x -> 1./(1. + exp (-2.*x)) @> "x" |]

let itype it (iris: Iris) = if it = iris.Type then 1.0 else 0.0 

let rec learn (input: Iris []) i network =
  if i > input.Length then 
    let x = input.[i].ToVector()
    let y = [|itype IrisSetosa input.[i]; itype IrisVersicolor input.[i]; itype IrisVirginica input.[i];|] |> DenseVector.ofArray
    learn input (i + 1) (network |> NeuralNetwork.learn (0.15, 0.1) x y )
  else 
    network

#time "on"
let network = NeuralNetwork.createWithHiddens 4 hiddens outputs |> learn (trainingData) 0
#time "off"

let iris = data.[0]
network |> NeuralNetwork.forward (iris.ToVector())
