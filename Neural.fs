module Neural

open System

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open MathNet.Numerics.LinearAlgebra
open Microsoft.FSharp.Quotations

open Utils.Misc
open Utils.Math.Differential


//TODO Custom differential -> Math.NET Symbolics
type Neuron = 
    {Weights: Vector<float>; [<NonSerialized>] Activation: float -> float; [<NonSerialized>] Activation': float -> float; Func: Expr<float -> float>}


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module Neuron = 
    open System.Runtime.Serialization.Formatters.Binary
    open System.IO

    let mutable log = fun (msg:string) -> ignore msg
    
    let inline normalize min max c = 
        (c - min)/(max - min)
    let inline costf (yh: float Vector) (y: float Vector) = 
        MathNet.Numerics.Distance.SSD(yh,y)

    let inline create weights bias func pName = 
        {   Weights = weights |> Vector.insert 0 bias; 
            Activation = func |> Utils.Misc.Expr.toLambda; 
            Activation' = func |> Utils.Math.Differential.d pName |> Utils.Misc.Expr.toLambda;
            Func = func   }

    let inline identity count = 
        {   Weights = DenseVector.create count 1.;
            Activation = id;
            Activation' = fun _ -> 1.;
            Func = <@ fun x -> x @>; }

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

    let inline learn (Y: float Vector) (X: float Matrix) (alpha, epsilon: float) (neuron: Neuron) = 
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

    let inline batchLearn (Y: float Vector) (X: float Matrix) batchSize (alpha, epsilon: float) (neuron: Neuron) = 
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

    let inline onlineLearning (error: float) (x: float Vector) alpha neuron = 
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

type Layer = Layer of Neuron []
    

type NeuralNetwork = Layer []

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module NeuralNetwork = 

    let inline create hiddenLayers output : NeuralNetwork = 
        let hiddens = hiddenLayers |> Array.Parallel.map(Layer)
        let output = Layer(output)
        [| hiddens; [|output|] |] |> Array.concat

    let rec private forward (input : float Vector) (network: NeuralNetwork) i results = 
        if i < network.Length then
            match network.[i] with
            | Layer(layer) -> 
                let result = layer |> Array.Parallel.map(Neuron.forward input) |> DenseVector.ofArray
                forward result network (i + 1) (result::results)
        else 
            results.Head, results

    let inline forwardPropagation input network = 
        forward input network 0 [] |> fst

    let inline private backPropaganation errors layer