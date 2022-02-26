module SecondHW.Tests
open FsCheck
open SecondHW.NumberOfEvenNumbers

let numberOfEvenNumbersFunctionsShouldBeEqual x =
    let mapAndFilter = numberOfEvenNumbersMap x = numberOfEvenNumbersFilter x
    let mapAndFold = numberOfEvenNumbersMap x = numberOfEvenNumbersFold x
    let filterAndFold = numberOfEvenNumbersFilter x = numberOfEvenNumbersFold x
    match mapAndFilter, mapAndFold, filterAndFold with
    | true, true, true -> true
    | _ -> false

Check.QuickThrowOnFailure numberOfEvenNumbersFunctionsShouldBeEqual