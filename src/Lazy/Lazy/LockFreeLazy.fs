module Lazy.LockFreeLazy
open System.Threading
open ILazy

// LockFree implementation of ILazy
type LockFreeLazy<'t>(supplier: unit -> 't) =
    let mutable instance = None
    
    interface ILazy<'t> with
        member this.Get () =
            if instance.IsNone then
                let res = Some (supplier ())
                Interlocked.CompareExchange(&instance, res, None) |> ignore
                                
            instance.Value
