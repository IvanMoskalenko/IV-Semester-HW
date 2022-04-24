module Workflows.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let testWithRounding3 () =          
    let res =
        RoundingBuilder.rounding 3 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }

    res |> should equal 0.048

[<Test>]
let testWithNegativeRoundingAccuracy () =
    (fun () ->
        RoundingBuilder.rounding -1 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        } |> ignore) |> should throw typeof<System.ArgumentOutOfRangeException>

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