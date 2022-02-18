let factorial x =
    if x < 0 then failwith "Number mustn't be negative"
    if x = 0 then 1
    else
        let rec _go y acc = 
            if x = y then acc * y
            else _go (y + 1) (acc * y)
        _go 1 1
        
let fibonacci n =
    if n  1 then failwith "Number must be positive"
    else
        let rec _go x acc1 acc2 =
            if x = n then acc1
            else _go (x + 1) acc2 (acc1 + acc2)
        _go 1 0 1
        
let reverse list = 
    let rec _go lst acc =
        match lst with
        | head :: tail -> _go tail (head :: acc)
        | [] -> acc
    _go list []
    
let fourthTask n m =
    let rec _go x list =
        if x = m then list
        else _go (x + 1) ((list |> List.head |> (*) 2) :: list)
    _go 0 [pown 2 n] |> List.rev

let find list element =
    let rec _go list currentPosition =
        match list with
        | head :: tail ->
            if head = element then currentPosition
            else _go tail (currentPosition + 1)
        | [] -> failwith "Element is not in the list"
    _go list 0
    
printfn $"%A{find [6; 5; 3; 2; 6] 9}"