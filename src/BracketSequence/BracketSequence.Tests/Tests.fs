module BracketSequence.Tests

open NUnit.Framework
open FsUnit

[<TestCase("(test(dvrekvj)[eec]cdn{}rec())")>]
[<TestCase("f(bvjvbjfvbf()ncrv)rj")>]
[<TestCase("vnnvewkbjvebjkvjb")>]
[<TestCase("")>]
[<TestCase("(test(dvrekvj)[eec]cdn{}rec())")>]
let correctBracketSequenceTest string =
    string
    |> bracketSequenceChecker
    |> should be True

[<TestCase("bjdvdbjvdbj(sncnc{)}jwccb[]sc")>]
[<TestCase("sdsvjsdk)sndjvn")>]
let incorrectBracketSequenceTest string =
    string
    |> bracketSequenceChecker
    |> should be False