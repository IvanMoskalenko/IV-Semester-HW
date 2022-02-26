module SecondHW.Tests
open FsCheck
open SecondHW.NumberOfEvenNumbers
open SecondHW.BinaryTree
open SecondHW.ArithmeticTree
open SecondHW.PrimeNums
open FsUnit

let numberOfEvenNumbersFunctionsShouldBeEqual x =
    let mapAndFilter = numberOfEvenNumbersMap x = numberOfEvenNumbersFilter x
    let mapAndFold = numberOfEvenNumbersMap x = numberOfEvenNumbersFold x
    let filterAndFold = numberOfEvenNumbersFilter x = numberOfEvenNumbersFold x
    match mapAndFilter, mapAndFold, filterAndFold with
    | true, true, true -> true
    | _ -> false

Check.QuickThrowOnFailure numberOfEvenNumbersFunctionsShouldBeEqual

let tree =
    BinTree.Node(4, BinTree.Empty, BinTree.Node(2, BinTree.Empty, BinTree.Empty))

let newTree = mapTree tree (fun x -> x + 1)

newTree |> should equal (BinTree.Node(5, BinTree.Empty, BinTree.Node(3, BinTree.Empty, BinTree.Empty)))

let arithmeticTree =
    Node(Sum, Leaf 5, Node(Multiply, Leaf 3, Leaf 2))

let result = evaluate arithmeticTree
result |> should equal 11

let first10PrimeNums = [ for i in 0 .. 9 -> Seq.item i primeNums ]

first10PrimeNums |> should equal [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29 ]