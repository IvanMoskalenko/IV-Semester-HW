module PointFree.Tests

open NUnit.Framework
open FsCheck

[<Test>]
let func1AndFunc2ShouldBeEqual () =
    let check x list = func1 x list = func2 x list

    Check.QuickThrowOnFailure check

[<Test>]
let func1AndFunc3ShouldBeEqual () =
    let check x list = func1 x list = func3 x list

    Check.QuickThrowOnFailure check

[<Test>]
let func1AndFunc4ShouldBeEqual () =
    let check x list = func1 x list = func4 x list

    Check.QuickThrowOnFailure check

[<Test>]
let func1AndFunc5ShouldBeEqual () =
    let check x list = func1 x list = func5 x list

    Check.QuickThrowOnFailure check

[<Test>]
let func1AndFunc6ShouldBeEqual () =
    let check x list = func1 x list = func6 x list

    Check.QuickThrowOnFailure check
