module BracketSequence
open System

/// Checks the correctness of the brackets' placement
let bracketSequenceChecker string =    
    let bracketReverse bracket =
        if bracket = '}' then '{'
        elif bracket = ')' then '('
        elif bracket = ']' then '['
        else raise (ArgumentException "Wrong symbol. Only `}`, `)` and `]` can be reversed")
        
    let rec checkerInner sequence brackets =
        if (Seq.isEmpty sequence) then (List.isEmpty brackets) else
        match (Seq.head sequence) with
        | '(' | '{' | '[' as openingBracket -> checkerInner (Seq.tail sequence) (openingBracket :: brackets)
        | ')' | '}' | ']' as closingBracket ->
            match brackets with
            | bracket :: tl when bracket = (bracketReverse closingBracket) -> checkerInner (Seq.tail sequence) tl
            | _ -> false
        | _ -> checkerInner (Seq.tail sequence) brackets
    
    checkerInner string []
    