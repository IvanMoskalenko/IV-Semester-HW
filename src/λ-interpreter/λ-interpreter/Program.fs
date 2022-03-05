open System

type Variable = string
type Term =
    | Variable of Variable
    | Application of Term * Term
    | LambdaAbstraction of Variable * Term

let getNewName usedNames =
    let max: string = Set.maxElement usedNames
    let last = Seq.last max
    match Char.IsDigit last with
    | true -> max[0..max.Length - 2] + (last |> Char.GetNumericValue |> int |> (+) 1 |> string)
    | false -> max[0..max.Length - 1] + "0"

let rec freeVariables term =
    match term with
    | Variable x -> Set.singleton x
    | Application (term, term1) -> Set.union (freeVariables term) (freeVariables term1)
    | LambdaAbstraction(s, term) -> Set.difference (freeVariables term) (freeVariables (Variable s))


let check var var1 x y =
    if Set.contains var (freeVariables y) then false
    elif Set.contains var1 (freeVariables x) then false
    else true

let rec substitution variable x y =
    match (x, y) with
    | Variable s, _ when variable = s -> y
    | Variable _, _ -> x
    | Application (left, right), _ ->
        Application (substitution variable left y,
        substitution variable right y)
    | LambdaAbstraction (var, term), Variable _ when variable = var -> LambdaAbstraction (var, term)
    | LambdaAbstraction (var, term), _ when check var variable term y ->
        LambdaAbstraction (var, substitution variable term y)
    | LambdaAbstraction (var, term), _ -> failwith "todo"

        
let rec betaReduction term =
    match term with
    | Variable x -> Variable x
    | Application (LambdaAbstraction (x, term), y) -> substitution x term y
    | Application (x, y) ->
        let newLeft = betaReduction x
        match newLeft with
        | LambdaAbstraction (x, term) -> (x, term, y) |||> substitution |> betaReduction
        | _ -> Application(newLeft, betaReduction y)
    | LambdaAbstraction (x, y) -> LambdaAbstraction(x, betaReduction y)
    