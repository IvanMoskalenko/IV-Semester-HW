module Lazy.Tests
open FsUnit
open NUnit.Framework
open LazyFactory


[<Test>]
let SingleThreadedLazyShouldReturnFirstCalculation () =
    let random = System.Random ()
    let _lazy = LazyFactory.CreateLazy(random.NextInt64)
    let firstRes = _lazy.Get()
    let secondRes = _lazy.Get()
    firstRes |> should equal secondRes
        
        
[<Test>]
let threadSafeLazyShouldReturnFirstCalculation () =
    let mutable x = "DedSec256"
    let dedSecX2 () =
        x <- "DedSec512"
        x
    let _lazyLF = LazyFactory.CreateLockFreeLazy(dedSecX2)
    let _lazyMT = LazyFactory.CreateConcurrentLazy(dedSecX2)
    let getter = async { _lazyLF.Get() |> should equal "DedSec512"
                         _lazyMT.Get() |> should equal "DedSec512" }
    let getters = Seq.init 100 (fun _ -> getter)
    getters
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    
    x |> should equal "DedSec512"
    
[<Test>]
let threadSafeLazyCheckInSingleThreadedMode () =
    let x = 0
    let _lazyLF = LazyFactory.CreateLockFreeLazy(fun () -> x + 1)
    let _lazyMT = LazyFactory.CreateConcurrentLazy(fun () -> x + 1)
    _lazyLF.Get() |> should equal 1
    _lazyMT.Get() |> should equal 1
            