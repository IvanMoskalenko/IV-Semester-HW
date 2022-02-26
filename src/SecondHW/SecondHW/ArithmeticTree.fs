module SecondHW.ArithmeticTree

type Operation =
    | Sum
    | Subtraction
    | Multiply
    | Division

type ArithmeticTree<'t> =
    | Leaf of 't
    | Node of Operation * ArithmeticTree<'t> * ArithmeticTree<'t>

let rec evaluate tree =
    match tree with
    | Leaf x -> x
    | Node (operation, left, right) ->
        let left = evaluate left
        let right = evaluate right
        match operation with
        | Sum -> left + right
        | Subtraction -> left - right
        | Multiply -> left * right
        | Division -> left / right
