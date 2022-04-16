module Lazy.ILazy

/// Lazy evaluation interface
type ILazy<'a> =
    abstract member Get: unit -> 'a