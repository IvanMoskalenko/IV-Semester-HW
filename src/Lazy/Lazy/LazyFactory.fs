module Lazy.LazyFactory
open ILazy
open Lazy
open ConcurrentLazy
open LockFreeLazy

/// Factory that returns implementations of ILazy
type LazyFactory = 
    static member CreateLazy supplier = 
        new Lazy<'t>(supplier) :> ILazy<'t>

    static member CreateLockFreeLazy supplier = 
        new LockFreeLazy<'t>(supplier) :> ILazy<'t>

    static member CreateConcurrentLazy supplier = 
        new ConcurrentLazy<'t>(supplier) :> ILazy<'t>
