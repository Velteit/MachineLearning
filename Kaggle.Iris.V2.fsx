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

let createNeuron alpha data itype =
    let weights = [rgen.NextDouble(); rgen.NextDouble(); rgen.NextDouble(); rgen.NextDouble()] |> DenseVector.ofList
    let expected = data |> Array.Parallel.map (fun iris -> if iris.Type = itype then 1. else 0.) |> SparseVector.ofArray
    let input = data |> Array.Parallel.map (fun iris -> iris.ToVector()) |> List.ofArray |> DenseMatrix.ofRows
    Neuron.create (weights) (rgen.NextDouble()) <@ fun x -> 1./(1. + exp (-2.*x))  @> "x"
    |> Neuron.learn expected input (alpha, (fun yh y -> ((yh - y) |> Vector.fold(fun s r -> s + pown r 2) 0.) * 0.5), 0.1)

let createNeuronBatch alpha batchSize data itype =
    let weights = [rgen.NextDouble(); rgen.NextDouble(); rgen.NextDouble(); rgen.NextDouble()] |> DenseVector.ofList
    let expected = data |> Array.Parallel.map (fun iris -> if iris.Type = itype then 1. else 0.) |> SparseVector.ofArray
    let input = data |> Array.Parallel.map (fun iris -> iris.ToVector()) |> List.ofArray |> DenseMatrix.ofRows
    Neuron.create (weights) (rgen.NextDouble()) <@ fun x -> 1./(1. + exp (-2.*x))  @> "x"
    |> Neuron.batchLearn expected input batchSize (alpha, (fun yh y -> ((yh - y) |> Vector.map(fun r -> pown r 2) |> Vector.sum) * 0.5), 0.1)



let setosaPath = Path.Combine(__SOURCE_DIRECTORY__, "setosa.bin")
let virginicaPath = Path.Combine(__SOURCE_DIRECTORY__, "virginica.bin")
let versicolorPath = Path.Combine(__SOURCE_DIRECTORY__, "versicolor.bin")

//setosaNeuron |> Neuron.save path
//let test = Neuron.load path

#time "on"
let (setosaNeuron, setotsaErrors) =
    createNeuron 0.15 trainingData IrisSetosa
#time "off"

[rgen.NextDouble(); rgen.NextDouble(); rgen.NextDouble(); rgen.NextDouble()] |> DenseVector.ofList
data |> Array.Parallel.map (fun iris -> if iris.Type = IrisVersicolor then 1. else 0.) |> SparseVector.ofArray
data |> Array.Parallel.map (fun iris -> iris.ToVector()) |> List.ofArray |> DenseMatrix.ofRows


data
|> Array.Parallel.map(fun iris -> iris.Type, setosaNeuron |> Neuron.forward (iris.ToVector()))
|> Array.filter(fun (l, r) -> l = IrisSetosa)


let (versicolorNeuron, versicolorErrors) =
    createNeuronBatch 0.5 70 trainingData IrisVersicolor

data
|> Array.Parallel.map(fun iris -> iris.Type, versicolorNeuron |> Neuron.forward (iris.ToVector()))
|> Array.filter(fun (l,r) -> r > 0.)

Neuron.log <- printfn "%s"
Neuron.log <- ignore

let thread = System.Threading.Thread(fun () ->
    let (virginicaNeuron, virginicaErrors) =
        createNeuron 0.777 (Array.concat [|trainingVirginica; trainingSetosa; trainingVersicolor|]) IrisVirginica
    let virginicaPath = Path.Combine(__SOURCE_DIRECTORY__, "virginica.bin")
    virginicaNeuron |> Neuron.save virginicaPath
)
thread.Start()
thread.Abort()

    //createNeuronBatch 1.9 50 trainingData IrisVirginica

data
|> Array.Parallel.map(fun iris -> let v = iris.ToVector() in iris.Type, virginicaNeuron |> Neuron.forward (v))
|> Array.filter(fun (i,vr) -> i = IrisVirginica && vr < 0.)

data
|> Array.Parallel.map(fun iris -> let v = iris.ToVector() in iris.Type, virginicaNeuron |> Neuron.forward (v), setosaNeuron |> Neuron.forward v)
|> Array.filter(fun (i, vr, s) -> i = IrisSetosa || i = IrisVirginica)

let v = [1.; 2.; 3.; 4.] |> DenseVector.ofList
v.PointwiseMultiply(v).DotProduct(v)

setotsaErrors
|> List.rev
|> Series.ofValues
|> Chart.Line

[8; 6; 5; 4; 4; 4; 4; 4; 4; 3; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1] |> List.sum


