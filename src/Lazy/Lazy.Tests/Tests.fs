module Lazy.Tests
open System.Threading
open FsUnit
open NUnit.Framework
open LazyFactory


[<Test>]
let SingleThreadedLazyShouldReturnFirstCalculation () =
    let random = System.Random ()
    let _lazy = LazyFactory.CreateLazy(random.NextInt64)
    let firstRes = _lazy.Get()
    for _ in {1 .. 100} do
        let res = _lazy.Get()
        res |> should equal firstRes
        
        
[<Test>]
let lockFreeLazyShouldReturnFirstCalculation () =
    let mutable x = 0
    let _lazy = LazyFactory.CreateLockFreeLazy(fun () -> Interlocked.Increment &x)
    let getter = async { _lazy.Get() |> should equal 1 }
    let getters = Seq.init 100 (fun _ -> getter)
    getters
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    
    
[<Test>]
let multiThreadedLazyShouldReturnFirstCalculation () =
    let mutable x = 0
    let _lazy = LazyFactory.CreateConcurrentLazy(fun () -> Interlocked.Increment &x)
    let getter = async { _lazy.Get() |> should equal 1 }
    let getters = Seq.init 100 (fun _ -> getter)
    getters
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
    
    // Checks if the calculation was actually run once
    x |> should equal 1
    
        
    