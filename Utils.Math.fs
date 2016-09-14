module Utils.Math

open System

let inline (^) a b = Math.Pow(float a, float b)
let inline Σ fn array = array |> Array.sumBy fn

