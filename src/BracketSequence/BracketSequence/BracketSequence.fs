module BracketSequence
open System

/// Checks the correctness of the brackets' placement
let bracketSequenceChecker string =    
    let bracketReverse bracket =
        if bracket = '}' then '{'
        elif bracket = ')' then '('
        elif bracket = ']' then '['
        else raise (ArgumentException ())
        
    let rec checkerInner tail brackets =
        if (Seq.isEmpty tail) then (List.isEmpty brackets) else
        match (Seq.head tail) with
        | '(' | '{' | '[' as openingBracket -> checkerInner (Seq.tail tail) (openingBracket :: brackets)
        | ')' | '}' | ']' as closingBracket ->
            match brackets with
            | bracket :: tl when bracket = (bracketReverse closingBracket) -> checkerInner (Seq.tail tail) tl
            | _ -> false
        | _ -> checkerInner (Seq.tail tail) brackets
    
    checkerInner string []
    