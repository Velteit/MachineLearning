module Utils.Misc


module Array = 
    let shuffle (input: 'a []) =
        let rgen = System.Random()
        let input = Array.copy input
        let l = input.Length
        for i in (l-1) .. -1 .. 1 do
            let temp = input.[i]
            let j = rgen.Next(0, i+1)
            input.[i] <- input.[j]
            input.[j] <- temp
        input

module Expr = 
    open Microsoft.FSharp.Quotations

    open FSharp.Quotations.Evaluator

    let inline toLambda<'T> (expr: Expr) =
        expr |> QuotationEvaluator.CompileUntyped :?> 'T


module Combinators = 
    let inline forward (f: 'a -> 'b -> 'c) a = f a
    let inline forward2 (f: 'a -> 'b -> 'c) b = fun a -> f a b
    let inline rev f a b = f b a
    let inline ite predicate tF eF i = if predicate i then tF i else eF i
    
module Concurrent = 
    
    module CRef =
        [<Sealed>]
        type CRef<'T> (value: 'T) = 
            let mutable value = value
            let locker = obj()
            member internal __.Value with get () = value and set v = value <- v
            member internal __.Locker = locker

        let inline cref<'T> (value: 'T) = CRef<'T>(value)
        let set (cref: CRef<'a>) value = lock cref.Locker (fun () -> cref.Value <- value)
        let get (cref: CRef<'a>) = cref.Value