module PhoneBook

open System.IO

type BookRecord =
    { Name: string
      Phone: int64 }    

let addRecord name phone book =
    let record = { Name = name; Phone = phone }
    record :: book
    
let findPhone name book =
    (List.find (fun { Name = x; Phone = _ } -> x = name) book).Phone
    
let findName phone book =
    (List.find (fun { Name = _; Phone = x } -> x = phone) book).Name
    
let printBook book =
    List.iter (fun { Name = name; Phone = phone } -> printfn $"%s{name} %d{phone}") book
    
let saveBook (path: string) book =
    use streamWriter = new StreamWriter(path)
    List.iter (fun { Name = name; Phone = phone } -> streamWriter.WriteLine $"%s{name} %d{phone}") book
    
let readBook (path: string) =
    use streamReader = new StreamReader(path)
    let rec readBookInner acc =
        let line = streamReader.ReadLine()
        if line = null then acc
        else
            let split = line.Split(" ")
            if split.Length <> 2 then failwith "Invalid file"
            else
                let record = { Name = split[0]; Phone = int64 split[1] }
                readBookInner (record :: acc)
           
    readBookInner []
