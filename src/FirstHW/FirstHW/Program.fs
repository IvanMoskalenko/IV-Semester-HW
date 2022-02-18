module FirstHW
open System

/// <summary>
/// Calculates the factorial of a number
/// </summary>
let factorial x =
    if x < 0 then raise (ArgumentException("Number mustn't be negative"))
    if x = 0 then 1
    else
        let rec _go current acc = 
            if x = current then acc * current
            else _go (current + 1) (acc * current)
        _go 1 1

/// <summary>
/// Calculates the nth fibonacci number
/// </summary>        
let fibonacci n =
    if n < 1 then raise (ArgumentException("Number must be positive"))
    else
        let rec _go current acc1 acc2 =
            if current = n then acc1
            else _go (current + 1) acc2 (acc1 + acc2)
        _go 1 0 1

/// <summary>
/// Reverses list
/// </summary>           
let reverse list = 
    let rec _go lst acc =
        match lst with
        | head :: tail -> _go tail (head :: acc)
        | [] -> acc
    _go list []

/// <summary>
/// Gets numbers n and m. Returns [2^n; 2^(n + 1); ...; 2^(n + m)] list
/// </summary>   
let fourthTask n m =
    if n < 0 || m < 0 then raise (ArgumentException("n and m bust be non-negative"))
    let rec _go current list =
        if current = m then list
        else _go (current + 1) ((list |> List.head |> (*) 2) :: list)
    _go 0 [pown 2 n] |> List.rev

/// <summary>
/// Finds and returns the first occurrence of a number.
/// Raises exception if the element is not in the list
/// </summary>   
let find list element =
    let rec _go list currentPosition =
        match list with
        | head :: tail ->
            if head = element then currentPosition
            else _go tail (currentPosition + 1)
        | [] -> raise (ArgumentException("Element is not in the list"))
    _go list 0
    