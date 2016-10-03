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

  open Utils.Misc

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

  let inline weight j (neuron: Neuron) = 
    neuron.Weights.[j]

  let inline f (neuron: Neuron) = neuron.Activation
  let inline f' (neuron: Neuron) = neuron.Activation

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
    {neuron with Activation = neuron.Func |> Expr.compile; Activation' = neuron.Func |> Utils.Math.Differential.d pName |> Utils.Misc.Expr.compile ;}

type Layer =
    | Input of Neuron []
    | Hidden of Neuron []
    | Output of Neuron []


type NeuralNetwork = Layer []
    

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module NeuralNetwork =

  let neurons = function | Input(l) -> l | Hidden(l) -> l | Output(l) -> l

  let inline neuron l i (network: NeuralNetwork) = 
    match network.[l] with
    | Input(layer)  -> layer.[i]
    | Hidden(layer) -> layer.[i]
    | Output(layer) -> layer.[i]
    
  let inline weight l i j (network: NeuralNetwork) = 
    network |> neuron l i |> fun neuron -> neuron.Weights.[j]

  let inline createWithHiddens inputCount (hiddenLayers: Neuron [][]) (output : Neuron []) : NeuralNetwork =
    let input = Array.create inputCount (Neuron.identity 1)  |> Input
    [| [|input|]; (hiddenLayers |> Array.Parallel.map Hidden); [|Output(output)|] |] |> Array.concat

  let rec private forwardPropagation (input : float Vector) (network: NeuralNetwork) i results =
    if i < network.Length then
      let layer = network.[i]
      let result =
        match layer with 
        | Input(layer)    -> input
        | Hidden(layer)   -> layer |> Array.Parallel.map(Neuron.forward input) |> DenseVector.ofArray
        | Output(layer)   -> layer |> Array.Parallel.map(Neuron.forward input) |> DenseVector.ofArray
      forwardPropagation result network (i + 1) (result::results)
    else
      results.Head, results |> Array.ofList


  let inline  forward input network =
    forwardPropagation input network 0 [] |> fst

  let inline private learnLayer (alpha, epsilon) (inputs: float Vector) (errors: float []) layer =
    let learn i = if errors.[i] > epsilon then (Neuron.onlineLearning alpha inputs errors.[i]) else id
    match layer with 
    | Input(_) as il    -> il
    | Hidden(layer)     -> Hidden(layer |> Array.Parallel.mapi (fun i neuron -> neuron |>  learn i))
    | Output(layer)     -> Output(layer |> Array.Parallel.mapi (fun i neuron -> neuron |>  learn i))
    
    //  TODO Rewrite. comprehend the formula 
    // δ(OUT) = (yh - y)*f'(s)
    // δ(HID) = f'(s) * Σ(k)(δ_j*w_kj) k ϵ Child neurons
    // Δw_ij = - α * δ_j * x_i
    // ε
  (*let rec learn (alpha, epsilon) (X: float Vector) (Y: float Vector) (network: NeuralNetwork) =
    let Yh, Xs =
      forwardPropagation X network 0 []
    let se w errors = 
      w + (errors |> Array.sum)
    let dW i (neuron: Neuron) x errors = 
      x * se (neuron |> Neuron.weight i) errors
    let rec inner i errors =
      if i > 1 then
        let layer = network.[i]
        network.[i] <- learnLayer (alpha, epsilon) Xs.[i] errors layer
        let xs = Xs.[i]
        let fs' = network.[i - 1] |> neurons |> Array.map(Neuron.f')
        let currentNeurons = neurons layer 
        inner (i - 1) (fs' |> Array.mapi (fun i f -> dW i currentNeurons. (f xs.[i]) errors))
      else 
        network

    if costf Yh Y < epsilon then 
        network
    else
        inner (network.Length - 1) ((Yh - Y) |> Vector.toArray) 
        |> learn (alpha, epsilon) X Y *)