module SecondHW.NumberOfEvenNumbers

/// Calculates number of even numbers using List.map
let numberOfEvenNumbersMap list =
   list
   |> List.map (fun x -> if x % 2 = 0 then 1 else 0)
   |> List.sum

/// Calculates number of even numbers using List.filter
let numberOfEvenNumbersFilter list =
    list
    |> List.filter (fun x -> x % 2 = 0)
    |> List.length

/// Calculates number of even numbers using List.fold
let numberOfEvenNumbersFold list =
    (0, list)
    ||> List.fold (fun acc x -> if x % 2 = 0 then (acc + 1) else acc)

