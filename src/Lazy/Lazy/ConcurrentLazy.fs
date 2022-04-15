module Lazy.ConcurrentLazy
open System
open ILazy

type ConcurrentLazy<'t> (supplier: unit -> 't) =
    [<VolatileField>]
    let mutable instance = None
    let obj = Object ()
    
    interface ILazy<'t> with
        member this.Get () =
            if instance.IsNone then
                lock obj (fun () ->
                    if instance.IsNone then instance <- Some (supplier ()))

            instance.Value
