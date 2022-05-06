module MiniCrawler.Tests

open NUnit.Framework
open FsUnit
open MiniCrawler


[<Test>]
let noLinksTest () =
    let sizes = "https://www3.pioneer.com/argentina/e-CulProd/test.html"
                |> findSizes
                |> Async.RunSynchronously
                
    sizes |> should equivalent []    


[<Test>]
let singleLinkTest () =
    let sizes = "https://en.cppreference.com/w/"
                |> findSizes
                |> Async.RunSynchronously
    
    sizes |> should equivalent
                 [("https://en.cppreference.com/mwiki/index.php?title=Main_Page&amp;oldid=121738", Some 40250)]    

    
[<Test>]
let multipleLinksTest () =
    let sizes = "https://packages.ubuntu.com/impish/amd64/libc6"
                |> findSizes
                |> Async.RunSynchronously
        
    sizes |> should equivalent
                 [("https://launchpad.net/ubuntu/+source/glibc/+bugs", Some 162117)
                  ("https://bugs.launchpad.net/ubuntu/+source/glibc/+filebug", Some 1366)
                  ("https://answers.launchpad.net/ubuntu/+source/glibc/+addquestion", Some 1373)
                  ("https://www.gnu.org/software/libc/libc.html", Some 4691)
                  ("https://bugs.launchpad.net/pkg-website/+filebug", Some 1351)] 
    