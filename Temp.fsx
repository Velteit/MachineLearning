#r @"packages/FSharp.Quotations.Evaluator/lib/net40/FSharp.Quotations.Evaluator.dll"

#load "Utils.Misc.fs"

open Utils.Misc.Concurrent.CRef

let v = cref 10

let a i = async {set v ((get v) + i)}

[a 1; a 2; a 3; a 4]
|> Async.Parallel
|> Async.RunSynchronously

get v