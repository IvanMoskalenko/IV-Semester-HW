module PhoneBook

open System.IO

type BookRecord =
    { Name: string
      Phone: int64 }    

let addRecord name phone book =
    { Name = name; Phone = phone } :: book
    
let findPhone name =
    List.filter (fun { Name = x; Phone = _ } -> x = name)
    >> List.map (fun { Name = _; Phone = x } -> x)
    
let findName phone =
    List.filter (fun { Name = _; Phone = x } -> x = phone)
    >> List.map (fun { Name = x; Phone = _ } -> x)
    
let printBook =
    List.iter (fun { Name = name; Phone = phone } -> printfn $"%s{name} %d{phone}")
    
let saveBook (path: string) =
    use streamWriter = new StreamWriter(path)
    List.iter (fun { Name = name; Phone = phone } -> streamWriter.WriteLine $"%s{name} %d{phone}")
    
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
