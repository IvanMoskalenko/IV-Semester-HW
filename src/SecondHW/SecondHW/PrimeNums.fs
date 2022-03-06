module SecondHW.PrimeNums

/// Generates infinite sequence of prime nums
let primeNums =    
    let rec divisibilityTest item list =
        match list with
        | [] -> true
        | head :: tail when float head > (item |> float |> sqrt) -> divisibilityTest item tail
        | head :: tail when item % head <> 0 -> divisibilityTest item tail
        | _ -> false

    let rec inner n acc =
        seq {
            if divisibilityTest n acc then
                yield n
                yield! inner (n + 1) (n :: acc)
            else
                yield! inner (n + 1) acc
        }

    inner 2 []