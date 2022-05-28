module LocalNetwork

type OperationSystem =
    | Linux
    | Windows
    | MacOS

type Virus =
    { Name: string
      Linux: float
      Windows: float
      MacOS: float }

/// Represents one computer in the local network    
type Computer(name, operationSystem, viruses: Set<Virus>) =
    let mutable curViruses = viruses
    member this.Name = name
    member this.OperationSystem = operationSystem
    member this.Infect virus = curViruses <- curViruses.Add virus        
    member this.Viruses = curViruses

/// Represents local network    
type Network(computers: Computer[], connections: Map<int, int>, ?randomizer) =
    let mutable isStable = false
    let random =
        match randomizer with
        | Some value -> value
        | None -> System.Random()
    let adjacencyMatrix = Array2D.create computers.Length computers.Length false
    do
        connections |> Seq.iter (fun (KeyValue(f, s)) ->
            adjacencyMatrix[f, s] <- true
            adjacencyMatrix[s, f] <- true)
    member this.Computers = computers
    
    /// Calculates network's status after one step
    member this.OneStep =
        isStable <- true
        let infect index viruses =
            (viruses |> Seq.iter
            (fun virus ->
                let random = random.NextDouble()
                match this.Computers[index].OperationSystem with
                | Linux ->
                    if virus.Linux > 0. then isStable <- false
                    if random <= virus.Linux then
                        this.Computers[index].Infect virus
                | Windows ->
                    if virus.Windows > 0. then isStable <- false
                    if random <= virus.Windows then
                        this.Computers[index].Infect virus
                | MacOS ->
                    if virus.MacOS > 0. then isStable <- false
                    if random <= virus.MacOS then
                        this.Computers[index].Infect virus))
            
        let computersCopy = Array.copy this.Computers
        for i in [1 .. this.Computers.Length - 1] do
            for j in [0 .. i - 1] do
                if adjacencyMatrix[i, j]
                then
                    let diff1 = Set.difference computersCopy[i].Viruses computersCopy[j].Viruses
                    let diff2 = Set.difference computersCopy[j].Viruses computersCopy[i].Viruses
                    if (diff1, diff2) ||> Set.union |> Set.isEmpty |> not
                    then
                        infect j diff1
                        infect i diff2
    
    /// Calculates stable network status                
    member this.AllSteps =
        let mutable numOfSteps = 0
        while not isStable do
            this.OneStep
            numOfSteps <- numOfSteps + 1
            printfn $"Step %A{numOfSteps}. Network's status:"
            this.Computers |> Seq.iter (fun computer -> printfn $"%s{computer.Name} viruses: %A{computer.Viruses}")
        printfn "Network is stable."
        numOfSteps