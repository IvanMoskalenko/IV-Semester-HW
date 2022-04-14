module Test
open System

/// Finds min element in list
let findMin list =
    match list with
    | [] -> raise (ArgumentException())
    | _ -> List.fold min Int32.MaxValue list
 
/// Simple BinTree realisation   
type BinTree<'t> =
    | Empty
    | Node of 't * BinTree<'t> * BinTree<'t>

/// Returns elements that satisfy condition given    
let rec filterBinTree tree cond =
    match tree with
    | Empty -> []
    | Node (value, left, right) ->
        let leftRes = filterBinTree left cond
        let rightRes = filterBinTree right cond
        if cond value then value :: leftRes @ rightRes
        else leftRes @ rightRes
        
/// HashTable realisation       
type HashTable(hashFunc) =
    let mutable records = List.empty
    
    /// Adds KVP to HashTable
    member this.Add key value =
        let keyHash = hashFunc key
        if (List.tryFind (fun (hash, _, _) -> hash = keyHash) records) = None
        then records <- (keyHash, key, value) :: records
        else failwith "Key is already exists"
    
    /// Checks if HashTable contains KVP by key given    
    member this.ContainsKey key =
        let keyHash = hashFunc key
        match List.tryFind (fun (hash, _, _) -> hash = keyHash) records with
        | Some _ -> true
        | None -> false
    
    /// Checks if HashTable contains KVP by value given  
    member this.ContainsValue value =
        match List.tryFind (fun (_, _, _value) -> _value = value) records with
        | Some _ -> true
        | None -> false
    
    /// Removes element by key given
    member this.Delete key =
        if this.ContainsKey key then
            let keyHash = hashFunc key
            List.filter (fun (hash, _, _) -> hash <> keyHash) records
        else
            failwith "There is not element with this key"