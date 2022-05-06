module Lazy.Lazy
open ILazy

/// Simple implementation of ILazy
type Lazy<'t>(supplier: unit -> 't) =
    let mutable instance = None
    
    interface ILazy<'t> with
        member this.Get () =
            if instance.IsNone then instance <- (Some (supplier ()))

            instance.Value
        
    


