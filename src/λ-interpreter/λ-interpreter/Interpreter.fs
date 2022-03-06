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
    | LambdaAbstraction (x, term) -> Set.difference (freeVariables term) (x |> Variable |> freeVariables)


let check var var1 term term1 =
    if Set.contains var (freeVariables term1) then false
    elif Set.contains var1 (freeVariables term) then false
    else true

let rec substitution variable term1 term2 =
    match (term1, term2) with
    | Variable var, _ when variable = var -> term2
    | Variable _, _ -> term1
    | Application (leftTerm, rightTerm), _ ->
        Application (substitution variable leftTerm term2, substitution variable rightTerm term2)
    | LambdaAbstraction (var, term), Variable _ when variable = var -> LambdaAbstraction (var, term)
    | LambdaAbstraction (var, term), _ when check var variable term term2 ->
        LambdaAbstraction (var, substitution variable term term2)
    | LambdaAbstraction (var, term), _ ->
        let newVar = (freeVariables term, freeVariables term2) ||> Set.union |> getNewName
        let x = newVar |> Variable |> substitution var term
        let newTerm = substitution variable x term2
        LambdaAbstraction (newVar, newTerm)

        
let rec betaReduction term =
    match term with
    | Variable x -> Variable x
    | Application (LambdaAbstraction (variable, term1), term2) -> substitution variable term1 term2
    | Application (leftTerm, rightTerm) ->
        let newLeft = betaReduction leftTerm
        match newLeft with
        | LambdaAbstraction (variable, term) ->
            (variable, term, rightTerm) |||> substitution |> betaReduction
        | _ -> Application (newLeft, betaReduction rightTerm)
    | LambdaAbstraction (variable, term) -> LambdaAbstraction (variable, betaReduction term)


    