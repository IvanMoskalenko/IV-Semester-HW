module LocalNetwork.Tests

open NUnit.Framework
open FsUnit
open Foq

[<Test>]
let simpleTest () =
    let virus = { Name = "Ibra"; Linux = 0.1; Windows = 0.1; MacOS = 0.1 }
    let computer1 = Computer("Computer1", Linux, Set.singleton virus)
    let computer2 = Computer("Computer2", Linux, Set.empty)
    let computer3 = Computer("Computer3", Windows, Set.empty)
    let network = Network([|computer1; computer2; computer3|], Map[(0, 1); (1, 2)])
    network.AllSteps |> ignore
    network.Computers |> Seq.iter (fun computer -> computer.Viruses.Count |> should equal 1)
    
[<Test>]
let thirdComputerShouldNotBeInfected () =
    let virus = { Name = "Z"; Linux = 0.1; Windows = 0.0; MacOS = 0.1 }
    let computer1 = Computer("Computer1", Linux, Set.singleton virus)
    let computer2 = Computer("Computer2", Linux, Set.empty)
    let computer3 = Computer("Computer3", Windows, Set.empty)
    let network = Network([|computer1; computer2; computer3|], Map[(0, 1); (1, 2)])
    network.AllSteps |> ignore
    network.Computers[2].Viruses.Count |> should equal 0
    
[<Test>]
let numberOfStepsShouldBeStableWhenProbabilityIs1 () =
    let virus = { Name = "V"; Linux = 1.0; Windows = 1.0; MacOS = 1.0 }
    for _ in [1 .. 100] do
        let computer1 = Computer("Computer1", MacOS, Set.singleton virus)
        let computer2 = Computer("Computer2", Linux, Set.empty)
        let computer3 = Computer("Computer3", Windows, Set.empty)
        let network = Network([|computer1; computer2; computer3|], Map[(0, 1); (1, 2)])
        let numOfSteps = network.AllSteps
        numOfSteps |> should equal 2

[<Test>]                     
let testWithMock () =
    let virus1 = { Name = "O"; Linux = 0.8; Windows = 0.6; MacOS = 0.9 }
    for _ in [1 .. 100] do
        let computer1 = Computer("Computer1", MacOS, Set.empty)
        let computer2 = Computer("Computer2", Linux, Set.empty)
        let computer3 = Computer("Computer3", Windows, Set.singleton virus1)
        let computer4 = Computer("Computer4", MacOS, Set.empty)
        let randomizer = Mock<System.Random>()
                             .Setup(fun x -> <@ x.NextDouble() @>)
                             .Returns(0.7)
                             .Create()
        let network = Network([|computer1; computer2; computer3; computer4|], Map[(0, 1); (1, 2); (2, 3)], randomizer)
        let numOfSteps = network.AllSteps
        network.Computers[0].Viruses |> should equal (Set.singleton virus1)
        network.Computers[1].Viruses |> should equal (Set.singleton virus1)
        numOfSteps |> should equal 3