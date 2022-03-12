module PhoneBook.Tests

open NUnit.Framework
open FsUnit

let phoneBook =
    []
    |> addRecord "Ibragim" 1337L
    |> addRecord "Adzharik" 88005553535L

[<Test>]
let addRecordTest () =
    phoneBook
    |> addRecord "Mihail" 228228228
    |> should equal
        [ { Name = "Mihail"; Phone = 228228228L }
          { Name = "Adzharik"; Phone = 88005553535L }
          { Name = "Ibragim"; Phone = 1337L } ]

[<Test>]
let findPhoneTest () =
    phoneBook
    |> findPhone "Ibragim"
    |> should equal [ 1337L ]

[<Test>]
let findNameTest () =
    phoneBook
    |> findName 88005553535L
    |> should equal [ "Adzharik" ]

[<Test>]
let saveReadTest () =
    saveBook "test.txt" phoneBook
    readBook "test.txt" |> should equivalent phoneBook