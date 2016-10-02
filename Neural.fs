module Neural

open System

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open MathNet.Numerics.LinearAlgebra
open Microsoft.FSharp.Quotations

open Utils.Misc
open Utils.Math.Differential


//TODO Array functions to array module
//TODO Matrix rows array to module
type Neuron =
  {Weights: Vector<float>; [<NonSerialized>] Activation: float -> float; [<NonSerialized>] Activation': float -> float; Func: Expr<float -> float>}

let inline costf (yh: float Vector) (y: float Vector) =
  MathNet.Numerics.Distance.SSD(yh,y)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module Neuron =
  open System.Runtime.Serialization.Formatters.Binary
  open System.IO

  let inline normalize min max c =
    (c - min)/(max - min)

  let inline create weights bias func pName =
    { Weights = weights |> Vector.insert 0 bias;
      Activation = func |> Utils.Misc.Expr.compile;
      Activation' = func |> Utils.Math.Differential.d pName |> Utils.Misc.Expr.compile;
      Func = func }

  let inline identity count =
    {   Weights = DenseVector.create count 1.;
        Activation = id;
        Activation' = fun _ -> 1.;
        Func = <@ fun x -> x @>; }

  let inline forward x neuron =
    let min = x |> Vector.min
    let max = x |> Vector.max
    ((x |> Vector.map (normalize min max)) |> Vector.insert 0 1. |> neuron.Weights.DotProduct ) |> neuron.Activation

  let inline forwardMatrix (mx: float Matrix) (neuron: Neuron) =
    mx
    |> Matrix.toRowArrays
    |> Array.Parallel.map (fun vx -> let vx = vx |> DenseVector.ofArray in forward vx neuron)
    |> DenseVector.ofSeq

  let inline learn (alpha, epsilon: float) (X: float Matrix) (Y: float Vector) (neuron: Neuron) =
    let f' = neuron.Activation'
    let max = X |> Matrix.fold max (X.[0,0])
    let min = X |> Matrix.fold min (X.[0,0])
    let X' = X |> Matrix.map (normalize min max) |> Matrix.insertCol 0 (DenseVector.create X.RowCount 1.)
    let XT = X' |> Matrix.transpose
    let m = X.RowCount |> float
    let rec gdc neuron errors =
      let Yh = forwardMatrix X neuron
      let error = costf Yh Y
      if (error) < epsilon then
        neuron, errors
      else
        let Yd = (Yh - Y)
        let w' =
          XT
          |> Matrix.toRowArrays
          // alpha * Σ ((yh - y) * f'(wx) * x) = alpha * (Yh - Y) * f' (W * XT) * x
          |> Array.Parallel.map (fun x ->
            let x = x |> DenseVector.ofArray in
              Yd.PointwiseMultiply(x).DotProduct(X' * neuron.Weights |> Vector.map f') * (alpha/m))
          |> DenseVector.ofSeq
        gdc {neuron with Weights = neuron.Weights - w'} (error::errors)
    gdc neuron []

  let inline batchLearn (alpha, batchSize, epsilon: float) (X: float Matrix) (Y: float Vector) (neuron: Neuron) =
    let f' = neuron.Activation'
    let Y = Y.[0..batchSize]
    let X = X.[0..batchSize, 0..]
    let max = X |> Matrix.fold max (X.[0,0])
    let min = X |> Matrix.fold min (X.[0,0])
    let X' = X |> Matrix.map (normalize min max) |> Matrix.insertCol 0 (DenseVector.create X.RowCount 1.)
    let XT = X' |> Matrix.transpose
    let m = float batchSize
    let rec gdc neuron errors =
      let Yh = forwardMatrix X neuron
      let error = costf Yh Y
      if error < epsilon then
        neuron, errors
      else
        let Yd = (Yh - Y)
        let w' =
          XT
          |> Matrix.toRowArrays
          |> Array.Parallel.map (fun x ->
            let x = x |> DenseVector.ofArray in
              Yd.PointwiseMultiply(x).DotProduct(X' * neuron.Weights |> Vector.map f') * (alpha/m))
          |> DenseVector.ofSeq
        gdc {neuron with Weights = neuron.Weights - w'} (error::errors)
    gdc neuron []

  let inline onlineLearning alpha (x: float Vector) (error: float) neuron =
    let f' = neuron.Activation'
    let max = x |> Vector.fold max (x.[0])
    let min = x |> Vector.fold min (x.[0])
    let x' = x |> Vector.map (normalize min max) |> Vector.insert 0 1.
    let t = x'.DotProduct(neuron.Weights |> Vector.map f')
    let w' = neuron.Weights |> Vector.mapi (fun i wi -> wi -  (alpha) * (error) * t * x'.[i])
    {neuron with Weights = neuron.Weights - w'}

  let inline save path (neuron: Neuron) =
    let formatter = BinaryFormatter()
    use stream = new FileStream(path, FileMode.OpenOrCreate, FileAccess.Write)
    formatter.Serialize(stream, neuron)
    stream.Close()

  let inline load path =
    let formatter = BinaryFormatter()
    use stream = new FileStream(path, FileMode.Open, FileAccess.Read)
    let neuron = formatter.Deserialize(stream) :?> Neuron
    let pName = match neuron.Func with | Lambda(var, _) -> var.Name | _ -> "x"
    {neuron with Activation = neuron.Func |> Utils.Misc.Expr.toLambda; Activation' = neuron.Func |> Utils.Math.Differential.d pName |> Utils.Misc.Expr.toLambda;}

type Layer = Neuron []


type NeuralNetwork = Layer []

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module NeuralNetwork =

  let inline create hiddenLayers output : NeuralNetwork =
    [| hiddenLayers; [|output|] |] |> Array.concat

  let rec private forwardPropagation (input : float Vector) (network: NeuralNetwork) i results =
    if i < network.Length then
      let layer = network.[i]
      let result = layer |> Array.Parallel.map(Neuron.forward input) |> DenseVector.ofArray
      forwardPropagation result network (i + 1) (result::results)
    else
      results.Head, results |> Array.ofList


  let inline  forward input network =
    forwardPropagation input network 0 [] |> fst

  let inline private learnLayer (alpha, epsilon) (inputs: float Vector []) (errors: float []) (layer: Layer) =
    let learn i = if errors.[i] > epsilon then (Neuron.onlineLearning alpha inputs.[i] errors.[i]) else id
    layer |> Array.Parallel.mapiInPlace (fun i neuron -> neuron |>  learn i)

  let inline learn (alpha, epsilon) (X: float Matrix) (Y: float Vector) network =
    let rec inner i prevD =
      if i < network.Length then
        let layer = network.[i]
        let d = (Yh.[i] - Y.[i]) * ()
        let x = Xs.[i]
    let Yh, Xs =
      X
      |> Matrix.toRowArrays
      |> Array.Parallel.map(fun x ->
        network
        |> forwardPropagation x 0 [])
      |> fun result ->
        result |> Array.Parallel.map fst,
        result |> Array.collect snd
    if costf Yh Y < epsilon then
      network
    else
      for i=network.Length-1 to 0 do
