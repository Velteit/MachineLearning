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
    
    let inline normalize min max c = 
        (c - min)/(max - min)

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

    let inline forwardNormalized x neuron = 
        let min = x |> Vector.min
        let max = x |> Vector.max
        forward (x |> Vector.map (normalize min max)) neuron

    let inline forwardMatrixNormalized (mx: float Matrix) (neuron: Neuron) =
        let max = mx |> Matrix.fold max (mx.[0,0]) 
        let min = mx |> Matrix.fold min (mx.[0,0])
        mx 
        |> Matrix.map (normalize min max)
        |> Matrix.toRowArrays 
        |> Array.Parallel.map (fun vx -> let vx = vx |> DenseVector.ofArray in forward vx neuron) 
        |> DenseVector.ofSeq

    let inline learn (Y: float Vector) (X: float Matrix) (alpha, costf, epsilon: float) (neuron: Neuron) = 
        let f' = neuron.Activation'
        let max = X |> Matrix.fold max (X.[0,0]) 
        let min = X |> Matrix.fold min (X.[0,0])
        let X' = X |> Matrix.map (normalize min max) |> Matrix.insertCol 0 (DenseVector.create X.RowCount 1.)
        let XT = X' |> Matrix.transpose
        let m = X.RowCount |> float 
        let rec gdc neuron errors = 
            let Yh = forwardMatrix X neuron
            let error = costf Yh Y
            log (sprintf "costf: %f" error)
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

    let inline batchLearn (Y: float Vector) (X: float Matrix) batchSize (alpha, costf, epsilon: float) (neuron: Neuron) = 
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
            log (sprintf "costf: %f" error)
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

type Layer = 
    | Input of Neuron []
    | Hidden of Neuron []
    | Output of Neuron []

//type NeuralNetwork = Layer []

//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
//module NeuralNetwork = 

    
