module SecondHW.Tests
open FsCheck
open NUnit.Framework
open SecondHW.NumberOfEvenNumbers
open SecondHW.BinaryTree
open SecondHW.ArithmeticTree
open SecondHW.PrimeNums
open FsUnit

[<Test>]
let numberOfEvenNumbersTest () = 
    let numberOfEvenNumbersFunctionsShouldBeEqual x =
        let mapAndFilter = numberOfEvenNumbersMap x = numberOfEvenNumbersFilter x
        let mapAndFold = numberOfEvenNumbersMap x = numberOfEvenNumbersFold x
        let filterAndFold = numberOfEvenNumbersFilter x = numberOfEvenNumbersFold x
        match mapAndFilter, mapAndFold, filterAndFold with
        | true, true, true -> true
        | _ -> false

    Check.QuickThrowOnFailure numberOfEvenNumbersFunctionsShouldBeEqual

[<Test>]
let mapTreeTest () =
    let tree1 =
        BinTree.Node(4, BinTree.Empty, BinTree.Node(2, BinTree.Empty, BinTree.Empty))
    let newTree1 = mapTree tree1 (fun x -> x + 1)

    let tree2 =
        BinTree.Node(7, BinTree.Node(5, BinTree.Empty, BinTree.Empty), BinTree.Node(9, BinTree.Empty, BinTree.Empty))
    let newTree2 = mapTree tree2 (fun x -> x - 1)

    newTree1
    |> should equal (BinTree.Node(5, BinTree.Empty, BinTree.Node(3, BinTree.Empty, BinTree.Empty)))
    newTree2
    |> should equal
        (BinTree.Node(6, BinTree.Node(4, BinTree.Empty, BinTree.Empty), BinTree.Node(8, BinTree.Empty, BinTree.Empty)))

[<Test>]
let arithmeticTreeTest () =
    let arithmeticTree1 =
        Node(Sum, Leaf 5, Node(Multiply, Leaf 3, Leaf 2))
    let arithmeticTree2 =
        Node(Sum, Node(Subtraction, Leaf 7, Leaf 9), Node(Multiply, Leaf 3, Leaf 2))
    let arithmeticTree3 =
        Node(Multiply, Node(Division, Leaf 6, Leaf 2), Leaf 4)

    let result1 = evaluate arithmeticTree1
    let result2 = evaluate arithmeticTree2
    let result3 = evaluate arithmeticTree3
    
    result1 |> should equal 11
    result2 |> should equal 4
    result3 |> should equal 12

[<Test>]
let primeNumsTest () =
    let first10PrimeNums = [ for i in 0 .. 9 -> Seq.item i primeNums ]

    first10PrimeNums |> should equal [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29 ]