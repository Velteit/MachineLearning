module LazyList

open System.Collections
open System.Collections.Generic
open System.Runtime.InteropServices
open System

type LazyList<'T when 'T : equality> = 
    | Node of Head: 'T * Tail: Lazy<LazyList<'T>>
    | End
    interface IEnumerable<'T> with
        member x.GetEnumerator () =
            let gch = (ref >> GCHandle.Alloc) x
            {new IEnumerator<'T> with 
                member __.Current 
                    with get () = 
                        match !(gch.Target :?> Ref<LazyList<'T>>) with 
                        | Node(head, _) -> head 
                        | End -> Unchecked.defaultof<'T>
            interface IEnumerator with 
                member __.Current 
                    with get () = 
                        match !(gch.Target :?> Ref<LazyList<'T>>) with 
                        | Node(head, _) -> head 
                        | End -> Unchecked.defaultof<'T> 
                        :> obj
                member __.MoveNext () = 
                    match !(gch.Target :?> Ref<LazyList<'T>>) with 
                    | Node(_, tail)  -> 
                        (gch.Target :?> Ref<LazyList<'T>>) := (tail.Value)
                        match (tail.Value) with | Node(_,_) -> true | End -> false
                    | End -> false
                member __.Reset () = 
                    (gch.Target :?> Ref<LazyList<'T>>) := x
            interface IDisposable with
                member __.Dispose() = 
                    gch.Free() }
    interface IEnumerable with
        member x.GetEnumerator() =
            (x :> IEnumerable<'T>).GetEnumerator() :> IEnumerator


module LazyList = 
    let inline init<'T when 'T : equality> value (next: 'T -> 'T) (stop: 'T -> bool)=
        let rec inner value (next: 'T -> 'T) =
            if stop value  then End else Node(value, Lazy(fun () -> inner (next value) next))
        inner value next
    

