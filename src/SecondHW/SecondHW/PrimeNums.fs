module SecondHW.PrimeNums

/// Generates infinite sequence of prime nums
let primeNums =
    let rec helper item list =
        match list with
        | [] -> true
        | head :: tail when item % head <> 0 -> helper item tail
        | _ -> false

    let rec inner n acc =
        seq {
            if helper n acc then
                yield n
                yield! inner (n + 1) (n :: acc)
            else
                yield! inner (n + 1) acc
        }

    inner 2 []