module Interpreter.Tests
open NUnit.Framework
open FsUnit

[<Test>]
let standardExampleTest () =
    let value =
        betaReduction (
            Application(
                LambdaAbstraction("x", Variable "y"),
                Application(
                    LambdaAbstraction("x", Application(Variable "x", Application(Variable "x", Variable "x"))),
                    LambdaAbstraction("x", Application(Variable "x", Application(Variable "x", Variable "x")))
                )
            )
        )
    
    value |> should equal (Variable "y")

[<Test>]    
let simpleTest1 () =
    let value =
        betaReduction (Application(LambdaAbstraction("x", Variable "x"), Variable "z"))

    value |> should equal (Variable "z")
    
[<Test>]
let simpleTest2 () =
    let value =
        betaReduction (Application(LambdaAbstraction("x", Variable "z"), Variable "x"))

    value |> should equal (Variable "z")

[<Test>]
let testWithReplacement () =
    let value =
        betaReduction (Application(LambdaAbstraction("x", LambdaAbstraction("z", Variable "x")), Variable "x"))

    value |> should equal (LambdaAbstraction ("x0", Variable "x"))    
        
standardExampleTest ()
simpleTest1()
simpleTest2()
testWithReplacement ()
