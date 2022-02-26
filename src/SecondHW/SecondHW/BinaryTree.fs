module SecondHW.BinaryTree

type BinTree<'t> =
    | Empty
    | Node of 't * BinTree<'t> * BinTree<'t>

let rec mapTree tree func =
    match tree with
    | Empty -> Empty
    | Node (value, left, right) -> Node (func value, mapTree left func, mapTree right func)
