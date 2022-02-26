module SecondHW.ArithmeticTree

/// Simple arithmetic operations
type Operation =
    | Sum
    | Subtraction
    | Multiply
    | Division

/// Arithmetic expression parsing tree
type ArithmeticTree<'t> =
    | Leaf of 't
    | Node of Operation * ArithmeticTree<'t> * ArithmeticTree<'t>

/// Evaluates arithmetic expression represented by parsing tree
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
