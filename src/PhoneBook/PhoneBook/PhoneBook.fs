module PhoneBook

open System.IO

/// Represents one record in PhoneBook
type BookRecord =
    { Name: string
      Phone: int64 }    

/// Adds record to PhoneBook
let addRecord name phone book =
    { Name = name; Phone = phone } :: book

/// Finds all phones by name given    
let findPhone name =
    List.filter (fun { Name = x; Phone = _ } -> x = name)
    >> List.map (fun { Name = _; Phone = x } -> x)

/// Finds all names by phone given    
let findName phone =
    List.filter (fun { Name = _; Phone = x } -> x = phone)
    >> List.map (fun { Name = x; Phone = _ } -> x)

/// Prints PhoneBook records to console    
let printBook =
    List.iter (fun { Name = name; Phone = phone } -> printfn $"%s{name} %d{phone}")

/// Saves PhoneBook records to file    
let saveBook (path: string) book =
    use streamWriter = new StreamWriter(path)
    List.iter (fun { Name = name; Phone = phone } -> streamWriter.WriteLine $"%s{name} %d{phone}") book

/// Reads records from file    
let readBook (path: string) =
    use streamReader = new StreamReader(path)
    let rec readBookInner acc =
        let line = streamReader.ReadLine ()
        if line = null then acc
        else
            let split = line.Split " "
            if split.Length <> 2 then raise (FileLoadException())
            else
                let record = { Name = split[0]; Phone = int64 split[1] }
                readBookInner (record :: acc)
           
    readBookInner []
