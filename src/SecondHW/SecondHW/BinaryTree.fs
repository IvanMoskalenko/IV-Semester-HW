module SecondHW.BinaryTree

// Simple binary tree representation
type BinTree<'t> =
    | Empty
    | Node of 't * BinTree<'t> * BinTree<'t>

/// Map for binary tree
let rec mapTree tree func =
    match tree with
    | Empty -> Empty
    | Node (value, left, right) -> Node (func value, mapTree left func, mapTree right func)
