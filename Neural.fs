module Neural

open MathNet.Numerics.LinearAlgebra
open Microsoft.FSharp.Quotations

open Utils.Misc
open Utils.Math.Differential

type Neuron = 
    {Weights: Vector<float>; Activation: float -> float; Activation': float -> float }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix )>]
module Neuron = 
    let inline create (weights: float Vector) (fune: Expr) arg =
        {   Weights = weights |> Vector.insert 0 1.; 
            Activation = fune |> Expr.toLambda; 
            Activation' = fune |> d arg |> Expr.toLambda  }
        