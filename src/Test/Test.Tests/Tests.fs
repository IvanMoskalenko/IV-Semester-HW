module Test.Tests
open FsUnit
open NUnit.Framework

[<Test>]
let filterTreeTest () =
    let tree1 =
        Node(4, BinTree.Empty, BinTree.Node(2, BinTree.Empty, BinTree.Empty))
    let newTree1 = filterBinTree tree1 (fun x -> x <> 2)

    let tree2 =
        BinTree.Node(7, BinTree.Node(5, BinTree.Empty, BinTree.Empty), BinTree.Node(9, BinTree.Empty, BinTree.Empty))
    let newTree2 = filterBinTree tree2 (fun x -> x < 8)

    newTree1
    |> should equal [4]
    newTree2
    |> should equivalent [5; 7]
    
    
[<Test>]
let findMinTest () =
    let list1 = [-2; 4; 7; 3; 0]
    let list2 = [5; 5; 5; 2; 1; 228]
    
    findMin list1 |> should equal -2
    findMin list2 |> should equal 1
    
    