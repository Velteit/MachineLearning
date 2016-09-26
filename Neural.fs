module Neural

open MathNet.Numerics.LinearAlgebra
open Microsoft.FSharp.Quotations

open Utils.Misc
open Utils.Math.Differential

//TODO Custom differential -> Math.NET Symbolics
type Neuron = 
    {Weights: Vector<float>; Activation: float -> float; Activation': float -> float }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module Neuron = 
    let mutable log = fun (msg:string) -> ignore msg
    let inline create weights bias func pName = 
        {   Weights = weights |> Vector.insert 0 bias; 
            Activation = func |> Utils.Misc.Expr.toLambda; 
            Activation' = func |> Utils.Math.Differential.d pName |> Utils.Misc.Expr.toLambda   }

    let inline forward x neuron = 
        (x |> Vector.insert 0 1. |> neuron.Weights.DotProduct ) |> neuron.Activation

    let inline forwardMatrix (mx: float Matrix) (neuron: Neuron) =
        mx 
        |> Matrix.toRowArrays 
        |> Array.Parallel.map (fun vx -> let vx = vx |> DenseVector.ofArray in forward vx neuron) 
        |> DenseVector.ofSeq

    let (*inline*) learn (Y: float Vector) (X: float Matrix) (alpha, costf, epsilon: float) (neuron: Neuron) = 
    //TODO normalize
        let f = neuron.Activation
        let f' = neuron.Activation'
        let X' = X |> Matrix.insertCol 0 (DenseVector.create X.RowCount 1.)
        let XT = X' |> Matrix.transpose
        let m = X.RowCount |> float 
        let rec gdc neuron = 
            let Yh = forwardMatrix X neuron
            log (sprintf "costf: %f" (costf Yh Y))
            if (costf Yh Y) < epsilon then 
                neuron
            else
                let Yd = (Yh - Y) 
                let w' = 
                    XT  
                    |> Matrix.toRowArrays 
                    // alpha * Σ ((yh - y) * f'(wx) * x) = alpha * (Yh - Y) * f' (W * XT) * x
                    |> Array.(*Parallel.*)map (fun x -> 
                        let x = x |> DenseVector.ofArray in 
                            Yd.PointwiseMultiply(x).DotProduct(X' * neuron.Weights |> Vector.map f') * (alpha/m)) 
                    |> DenseVector.ofSeq                
                gdc {neuron with Weights = neuron.Weights - w'} 
        gdc neuron 


//type Layer = 
//    | Input of Neuron []
//    | Hidden of Neuron []
//    | Output of Neuron 
//
//type NeuralNetwork = Layer []

//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
//module NeuralNetwork = 

    
