module Lazy.LockFreeLazy
open System.Threading
open ILazy

type Lazy<'t> (supplier: unit -> 't) =
    let mutable instance = None
    let target = ref instance
    
    interface ILazy<'t> with
        member this.Get () =
            if instance.IsNone then
                let curVal = instance
                let res = Some (supplier ())
                Interlocked.CompareExchange(target, res, curVal) |> ignore
                
                
            instance.Value
