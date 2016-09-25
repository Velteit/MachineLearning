module Neural

open MathNet.Numerics.LinearAlgebra
open Microsoft.FSharp.Quotations

open Utils.Misc
open Utils.Math.Differential

let inline identity x = x
let inline one _ = 1. 
let transfer = function 
    | x when x <= 0. -> 0. 
    | x when x >= 1. -> 1. 
    | x -> x 
let sigmoid t x = 1./(1. + exp(-t*x))
let sigmoid' t x =  t*(sigmoid t x) * (1. - (sigmoid t) x) 
let th x = tanh x
let th' x = 1./pown (cosh x) 2

type Neuron = 
    {Weights: Vector<float>; Activation: float -> float; Activation': float -> float }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module Neuron = 
    let inline create (weights: float Vector) bias activation activation' =
        {   Weights = weights |> Vector.insert 0 bias; 
            Activation = activation
            Activation' = activation'   }

    let inline identity count =
        {   Weights = DenseVector.create count 1.; 
            Activation = id
            Activation' = fun _ -> 1.   } 

    let inline forward (input: float Vector) (neuron: Neuron) = 
        neuron.Weights.DotProduct (input |> Vector.insert 0 1.) 
        |> neuron.Activation

    let inline onlineLearn a input expected neuron = 
        let input = input |> Vector.insert 0 1.
        {neuron with Weights = (neuron.Weights |> Vector.mapi(fun i w -> w - a*(neuron |> forward input - expected)*(input.[i]) )) }

type Layer = 
    | Input of Neuron []
    | Hidden of Neuron []
    | Output of Neuron 

type NeuralNetwork = Layer []

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module NeuralNetwork = 
    let inline create count (hiddens: Neuron [][]) output : NeuralNetwork = 
        [|   [|Input(Array.create count (Neuron.identity 1))|]
             (hiddens |> Array.Parallel.map Hidden)
             [|Output(output)|]  |]
        |> Array.concat

    let inline forward (input: float Vector) (network: NeuralNetwork) = 
        let rec iter i (result: float Vector) =
            match network.[i] with
            | Input(neurons)        -> iter (i + 1) (neurons |> Array.Parallel.mapi(fun j neuron -> neuron |> Neuron.forward result.[j..j + 1]) |> DenseVector.ofArray)
            | Hidden(neurons)       -> iter (i + 1) (neurons |> Array.Parallel.map(fun neuron -> neuron |> Neuron.forward result) |> DenseVector.ofArray)
            | Output(neurons)       -> neurons |> Neuron.forward result
        iter 0 input

//    let inline learn a input expected network = 
//        let result = network |> forward input
//        
//        let rec backPropaganation result (network: NeuralNetwork) li = 
//            if li > 0 then
//                network.[li] <- 
//                    match network.[li] with
//                    | Hidden(neurons)     -> Hidden(neurons |> Array.Parallel.mapi(fun i neuron -> neuron))
//                    | Output(neuron)      -> 
//                backPropaganation result network (li - 1)
//            
//        backPropaganation result network (network.Length - 1)


module NeuralV2 = 
    //TODO Custom differential -> Math.NET Symbolics
    type Neuron = 
        Neuron of float Vector * (float -> float) * (float -> float)

    let inline create weights bias func pName = 
        let weights = weights |> Vector.insert 0 bias in
            let f = func |> Utils.Misc.Expr.toLambda  in
                let f' = func |> Utils.Math.Differential.d pName |> Utils.Misc.Expr.toLambda  in
                    Neuron(weights, f, f')

    let inline forward x = function
        | Neuron(weights, func, _) -> (x |> Vector.insert 0 1. |> weights.DotProduct ) |> func

    let inline forwardMatrix (mx: float Matrix) (neuron: Neuron) =
        mx |> Matrix.toRowSeq |> Seq.map (fun vx -> forward vx neuron) |> DenseVector.ofSeq


    let rec learn (Y: float Vector) (X: float Matrix) (alpha, costf, epsilon: float) (neuron: Neuron) = 
        let rec gdc neuron = 
            let Yh = forwardMatrix X neuron
            if (costf Yh Y) < epsilon then 
                neuron
            else 
                let (w, f, f') = neuron |> function | Neuron(w, f, f') -> w, f, f'
                let Yd = (Yh - Y) 
                let XT = X |> Matrix.insertCol 0 (DenseVector.create X.RowCount 1.)
                let w' = 
                    XT
                    |> Matrix.transpose
                    |> Matrix.toRowSeq 
                    // alpha * Σ ((yh - y) * f'(wx) * x) = alpha * (Yh - Y) * f' (W * XT) * x
                    |> Seq.map (fun x -> Yd.PointwiseMultiply(x).DotProduct(XT * w |> Vector.map f') * alpha) 
                    |> DenseVector.ofSeq
                Neuron(w - w', f, f') |> gdc 
        gdc neuron 
