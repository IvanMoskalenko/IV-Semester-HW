module MiniCrawler

open System.Net.Http
open System.Text.RegularExpressions

/// Downloads page in HTML format
let downloadPage (client: HttpClient) (url: string) =
    client.GetStringAsync(url)
    |> Async.AwaitTask
    |> Async.Catch

/// Finds URLs in HTML
let findURLs html =
    let urlPattern = "<a href=\"(https://\S+)\">"
    let regex = Regex(urlPattern, RegexOptions.Compiled)

    regex.Matches(html)
    |> Seq.map (fun x -> x.Groups[1].Value)

/// Finds the sizes of pages that are linked from a given URL
let findSizes url =
    async {
    let client = new HttpClient()
    match (client, url) ||> downloadPage |> Async.RunSynchronously with
    | Choice1Of2 content ->
        let urls = findURLs content
        return (urls
        |> Seq.map (downloadPage client)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.map
            (fun response ->
                match response with
                | Choice1Of2 content -> Some content.Length
                | Choice2Of2 _ -> None)
        |> Seq.zip urls)
    | Choice2Of2 _ -> return Seq.empty
    }

/// Prints the sizes of pages in "URL — size" format
let printSizes urlResTuples =
    Seq.iter
        (fun (url, res) ->
            match res with
            | Some value -> printfn $"%s{url} — %i{value}"
            | None -> printfn $"Error was occured while downloading %s{url}")
        urlResTuples
