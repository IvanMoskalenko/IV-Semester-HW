module MiniCrawler.Tests

open NUnit.Framework
open FsUnit
open MiniCrawler
    
[<Test>]
let stackOverflowDownloadTest () =
    let sizes = findSizes "https://stackoverflow.com/"
    sizes |> Seq.length |> should equal 5

    Seq.iter
        (fun (url, res) ->
            match res with
            | Some value ->
                match url with
                | "https://stackoverflow.com" ->
                    value |> should be (greaterThan 17e4)
                    value |> should be (lessThan 18e4)
                | "https://stackexchange.com/sites" ->
                    value |> should be (greaterThan 48e4)
                    value |> should be (lessThan 49e4)
                | "https://stackoverflow.com/help/licensing" ->
                    value |> should be (greaterThan 5e4)
                    value |> should be (lessThan 6e4)
                | "https://stackoverflow.com/legal/cookie-policy" ->
                    value |> should be (greaterThan 9e4)
                    value |> should be (lessThan 1e5)
                | _ -> Assert.Fail()
            | None -> Assert.Fail())
        sizes
    
    
[<Test>]
let open3dDocsTest () =
    let sizes = findSizes "http://www.open3d.org/docs/release/python_api/open3d.camera.html"
    sizes |> Seq.length |> should equal 2
    
    Seq.iter
        (fun (url, res) ->
            match res with
            | Some value ->
                match url with
                | "https://github.com/rtfd/sphinx_rtd_theme" ->
                    value |> should be (greaterThan 2e5)
                    value |> should be (lessThan 22e4)
                | "https://readthedocs.org" ->
                    value |> should be (greaterThan 19e3)
                    value |> should be (lessThan 21e3)
                | _ -> Assert.Fail()
            | None -> Assert.Fail())
        sizes
    