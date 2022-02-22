module FirstHW
open System

/// Calculates the factorial of a number
let factorial x =
    if x < 0 then raise (ArgumentException("Number mustn't be negative"))
    if x = 0 then 1
    else
        let rec factorialInternal current acc = 
            if x = current then acc * current
            else factorialInternal (current + 1) (acc * current)
        factorialInternal 1 1

/// Calculates the nth fibonacci number    
let fibonacci n =
    if n < 1 then raise (ArgumentException("Number must be positive")) else       
        let rec fibonacciInternal current acc1 acc2 =
            if current = n then acc1
            else fibonacciInternal (current + 1) acc2 (acc1 + acc2)
        fibonacciInternal 1 0 1

/// Reverses list        
let reverse list = 
    let rec reverseInternal lst acc =
        match lst with
        | head :: tail -> reverseInternal tail (head :: acc)
        | [] -> acc
    reverseInternal list []

/// Gets numbers n and m. Returns [2^n; 2^(n + 1); ...; 2^(n + m)] list 
let fourthTask n m =
    if n < 0 || m < 0 then raise (ArgumentException("n and m must be non-negative"))
    let rec helper current list =
        if current = m then list
        else helper (current + 1) ((list |> List.head |> (*) 2) :: list)
    helper 0 [pown 2 n] |> List.rev

/// Finds and returns the first occurrence of a number.
/// Raises exception if the element is not in the list 
let find list element =
    let rec findInternal list currentPosition =
        match list with
        | head :: tail ->
            if head = element then currentPosition
            else findInternal tail (currentPosition + 1)
        | [] -> raise (ArgumentException("Element is not in the list"))
    findInternal list 0

    