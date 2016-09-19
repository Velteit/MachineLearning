module Utils.Math

open System

let inline (^) a b = Math.Pow(float a, float b)
let inline Σ fn array = array |> Array.sumBy fn

module Differential = 
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    
    //TODO System.Math.*
    let inline d pName ``function`` = 
      let rec inner expr =
        match expr with
        | SpecificCall <@@   (+)   @@> (_, _, exprList)       -> let u = exprList.Head in let v = exprList.Tail.Head in <@ (%inner u) + (%inner v) @>
        | SpecificCall <@@   (-)   @@> (_, _, exprList)       -> let u = exprList.Head in let v = exprList.Tail.Head in <@ (%inner u) - (%inner v) @>
        | SpecificCall <@@  (~-)   @@> (_, _, exprList)       -> let u = exprList.Head in <@ -(%inner u) @>
        | SpecificCall <@@   (*)   @@> (_, _, exprList)       -> let u = exprList.Head in let v = exprList.Tail.Head in <@ (%inner u)*(%%v) + (%%u)*(%inner v) @>
        | SpecificCall <@@   (/)   @@> (_, _, exprList)       -> let u = exprList.Head in let v = exprList.Tail.Head in <@ ((%inner u)*(%%v) - (%%u)*(%inner v))/(Math.Pow(%%v, 2.)) @>
        | SpecificCall <@@  (exp)  @@> (_, _, exprList)       -> <@ (%inner exprList.Head)*(exp %%exprList.Head) @>
        | SpecificCall <@@  (sqrt) @@> (_, _, exprList)       -> <@ (%inner exprList.Head)/(2.*(sqrt %%exprList.Head)) @>
        | SpecificCall <@@  (log)  @@> (_, _, exprList)       -> <@ (%inner exprList.Head)/(%%exprList.Head) @>
        | SpecificCall <@@  (sin)  @@> (_, _, exprList)       -> <@ (%inner exprList.Head)*cos(%%exprList.Head) @>
        | SpecificCall <@@  (cos)  @@> (_, _, exprList)       -> <@ -(%inner exprList.Head)*sin(%%exprList.Head) @>
        | SpecificCall <@@  (tan)  @@> (_, _, exprList)       -> <@ (%inner exprList.Head)/(Math.Pow(cos %%exprList.Head, 2.)) @>
        | SpecificCall <@@  (pown) @@> (_, _, exprList)       -> failwith "Use Math.Pow instead"
        //TODO Bug in quotations or in Quotations.Evaluator?
        //<@ (float %%exprList.Tail.Head)*(%inner exprList.Head)*(pown %%exprList.Head (%%exprList.Tail.Head - 1)) @>
        | SpecificCall <@@  (cosh) @@> (_, _, exprList)       -> <@ (%inner exprList.Head)*sinh(%%exprList.Head) @>
        | SpecificCall <@@  (sinh) @@> (_, _, exprList)       -> <@ (%inner exprList.Head)*cosh(%%exprList.Head) @>
        | SpecificCall <@@  (tanh) @@> (_, _, exprList)       -> <@ (%inner exprList.Head)/(Math.Pow(cosh %%exprList.Head, 2.)) @>
        | SpecificCall <@@  (atan) @@> (_, _, exprList)       -> <@ (%inner exprList.Head)/(1. + Math.Pow(%%exprList.Head, 2.)) @>
        | SpecificCall <@@  (acos) @@> (_, _, exprList)       -> <@ -(%inner exprList.Head)/sqrt (1. - Math.Pow(%%exprList.Head, 2.)) @>
        | SpecificCall <@@  (asin) @@> (_, _, exprList)       -> <@ (%inner exprList.Head)/sqrt (1. - Math.Pow(%%exprList.Head, 2.)) @>
        | SpecificCall <@@  (Math.Exp)  @@> (_, _, exprList)  -> <@ (%inner exprList.Head)*(Math.Exp %%exprList.Head) @>
        | SpecificCall <@@  (Math.Sqrt) @@> (_, _, exprList)  -> <@ (%inner exprList.Head)/(2.*(Math.Sqrt %%exprList.Head)) @>
        | SpecificCall <@@  (Math.Log)  @@> (_, _, exprList)  -> <@ (%inner exprList.Head)/(%%exprList.Head) @>
        | SpecificCall <@@  (Math.Pow)  @@> (_, _, exprList)  -> <@ (%%exprList.Tail.Head)*(%inner exprList.Head)*Math.Pow(%%exprList.Head, (%%exprList.Tail.Head - 1.)) @>
        | SpecificCall <@@  (Math.Sin)  @@> (_, _, exprList)  -> <@ (%inner exprList.Head)*Math.Cos(%%exprList.Head) @>
        | SpecificCall <@@  (Math.Cos)  @@> (_, _, exprList)  -> <@ -(%inner exprList.Head)*Math.Sin(%%exprList.Head) @>
        | SpecificCall <@@  (Math.Tan)  @@> (_, _, exprList)  -> <@ (%inner exprList.Head)/Math.Pow(Math.Cos %%exprList.Head, 2.) @>
        | SpecificCall <@@  (Math.Cosh) @@> (_, _, exprList)  -> <@ (%inner exprList.Head)*Math.Sinh(%%exprList.Head) @>
        | SpecificCall <@@  (Math.Sinh) @@> (_, _, exprList)  -> <@ (%inner exprList.Head)*Math.Cosh(%%exprList.Head) @>
        | SpecificCall <@@  (Math.Tanh) @@> (_, _, exprList)  -> <@ (%inner exprList.Head)/Math.Pow(Math.Cosh %%exprList.Head, 2.) @>
        | SpecificCall <@@  (Math.Atan) @@> (_, _, exprList)  -> <@ (%inner exprList.Head)/(1. + Math.Pow(%%exprList.Head, 2.)) @>
        | SpecificCall <@@  (Math.Acos) @@> (_, _, exprList)  -> <@ -(%inner exprList.Head)/Math.Sqrt(1. - Math.Pow(%%exprList.Head, 2.)) @>
        | SpecificCall <@@  (Math.Asin) @@> (_, _, exprList)  -> <@ (%inner exprList.Head)/Math.Sqrt (1. - Math.Pow(%%exprList.Head, 2.)) @>
        | Var(v)                                              -> if v.Name = pName then <@ 1. @> else <@ 0. @>
        | Value(_)                                            -> <@ 0. @>
        | _                                                   -> failwith "Error"
      let rec createMultiParamLambda input =
        match input with
        | Lambda(var, body) -> Expr.Lambda(var, createMultiParamLambda body)
        | _                 -> <@ %inner input @> :> Expr
      createMultiParamLambda ``function``