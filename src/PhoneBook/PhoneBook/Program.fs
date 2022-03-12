open System
open System.IO
open PhoneBook

let printHelp () =
    printfn
        "%s"
        "Hello!\n\
        Available commands for PhoneBook:\n\
        1 — exit\n\
        2 — add new record to PhoneBook\n\
        3 — find phone number by name\n\
        4 — find name by phone\n\
        5 — print current book to console\n\
        6 — save book to file\n\
        7 — read book from file\n\
        Any other command — print this message to console one more time"

let getName () =
    printf "%s" "Please, enter name: "
    Console.ReadLine()

let rec getPhone () =
    printf "%s" "Please, enter phone: "
    try
        Console.ReadLine() |> int64
    with
    | :? FormatException ->
        Console.WriteLine "Phone number was in incorrect format, try again"
        getPhone ()

let rec getValidPathToLoad () =
    printf "%s" "Please, enter path: "
    let path = Console.ReadLine()
    if File.Exists path then path
    else
        printfn "%s" "Path doesn't exist, try again"
        getValidPathToLoad ()

let rec getValidPathToSave () =
    printf "%s" "Please, enter path: "
    let path = Console.ReadLine()
    let directoryExists =
        path |> Path.GetDirectoryName |> Directory.Exists
    if directoryExists then path
    else
        printfn "%s" "Directory doesn't exist, try again"
        getValidPathToSave ()

let rec program book =
    printf "%s" "Please, enter command: "
    match Console.ReadLine() with
    | "1" -> ()
    | "2" -> program (addRecord (getName ()) (getPhone ()) book)
    | "3" ->
        List.iter (fun x -> printfn $"%d{x}") (findPhone (getName ()) book)
        program book
    | "4" ->
        List.iter (fun x -> printfn $"%s{x}") (findName (getPhone ()) book)
        program book
    | "5" ->
        printBook book
        program book
    | "6" ->
        saveBook (getValidPathToSave ()) book
        program book
    | "7" ->
        try
            program (readBook (getValidPathToLoad ()))
        with
        | :? FileLoadException | :? FormatException ->
            printfn "%s" "Invalid file. Records must be in {name: string} {phone: int) format"
            program book
    | _ ->
        printHelp ()
        program book

printHelp ()
program []