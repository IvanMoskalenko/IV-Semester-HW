module BracketSequence.Tests
open NUnit.Framework
open FsUnit

[<Test>]
let correctStringShouldReturnTrue1 () =
    "(test(dvrekvj)[eec]cdn{}rec())"
    |> bracketSequenceChecker
    |> should be True

[<Test>]
let correctStringShouldReturnTrue2 () =
    "f(bvjvbjfvbf()ncrv)rj"
    |> bracketSequenceChecker
    |> should be True

[<Test>]
let stringWithoutBracketsShouldReturnTrue () =
    "vnnvewkbjvebjkvjb"
    |> bracketSequenceChecker
    |> should be True

[<Test>]
let emptyStringShouldReturnTrue () =
    ""
    |> bracketSequenceChecker
    |> should be True

[<Test>]
let incorrectStringShouldReturnFalse () =
    "bjdvdbjvdbj(sncnc{)}jwccb[]sc"
    |> bracketSequenceChecker
    |> should be False

[<Test>]
let incorrectStringShouldReturnFalse2 () =
    "sdsvjsdk)sndjvn"
    |> bracketSequenceChecker
    |> should be False