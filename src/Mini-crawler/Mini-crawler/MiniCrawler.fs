module MiniCrawler

open System.Net.Http
open System.Text.RegularExpressions

let downloadPage (url: string) =
    async {
        try 
            use client = new HttpClient()
            let! content = client.GetStringAsync(url) |> Async.AwaitTask
            return Some content
        with
        | _ -> return None        
    }
    
let findURLs html =
    let pattern1 = "<a href=\"https://\S+\">"
    let pattern2 = "https://\S+\""

    Regex.Matches(html, pattern1)
    |> Seq.map
        (fun x ->
            let url = Regex.Match(x.Value, pattern2).Value
            url[0 .. url.Length - 2])

let findSizes url =
    match url |> downloadPage |> Async.RunSynchronously with
    | Some content ->
        let urls = findURLs content
        urls
        |> Seq.map downloadPage
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.map
            (fun response ->
                match response with
                | Some content -> Some content.Length
                | None -> None)
        |> Seq.zip urls
    | _ -> Seq.empty

let printSizes urlResTuples =
    Seq.iter
        (fun (url, res) ->
            match res with
            | Some value -> printfn $"%s{url} — %i{value}"
            | None -> printfn $"Error was occured while downloading %s{url}")
        urlResTuples
