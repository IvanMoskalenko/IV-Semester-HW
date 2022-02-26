module SecondHW.NumberOfEvenNumbers

let numberOfEvenNumbersMap list =
    List.sum list
    - (list
       |> List.map (fun x -> if x % 2 = 0 then x - 1 else x)
       |> List.sum)

let numberOfEvenNumbersFilter list =
    list
    |> List.filter (fun x -> x % 2 = 0)
    |> List.length

let numberOfEvenNumbersFold list =
    (0, list)
    ||> List.fold (fun acc x -> if x % 2 = 0 then (acc + 1) else acc)

