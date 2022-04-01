module Workflows.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let testWithRounding3 () =
    let rounding = RoundingBuilder(3)
    
    let res =
        rounding {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }

    res |> should equal 0.048

[<Test>]
let testWithRounding2 () =
    let rounding = RoundingBuilder(2)

    let res =
        rounding {
            let! a = 22.8 * 13.37
            let! b = 24.02
            return a + b
        }

    res |> should equal 328.86

[<Test>]
let testForCorrectString1 () =
    let calculate = StringCalculatingBuilder()

    let res =
        calculate {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }

    res |> should equal (Some 3)

[<Test>]
let testForCorrectString2 () =
    let calculate = StringCalculatingBuilder()

    let res =
        calculate {
            let! x = "4"
            let! y = "2"
            let! z = "10"
            let a = x / y + z
            return a
        }

    res |> should equal (Some 12)

[<Test>]
let testForIncorrectString () =
    let calculate = StringCalculatingBuilder()

    let res =
        calculate {
            let! x = "4"
            let! y = "Z"
            let! z = "10"
            let a = x / y + z
            return a
        }

    res |> should equal None