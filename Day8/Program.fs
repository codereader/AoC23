open Day8
open System

// Real puzzle input
let lines = IO.File.ReadAllLines @"..\..\..\input.txt"

let instructions = lines[0]

let mapLines =
    lines
    |> Seq.skip 2

let map =
    mapLines
    |> Seq.map Location
    |> Seq.map (fun location -> location.Name, location)
    |> dict

//printfn "%A" map

let CalculateStepsToReachGoal(startLocation:string, goalReached) = 
    let mutable steps = 0
    let mutable nextInstruction = 0
    let mutable currentLocation = startLocation

    while (goalReached currentLocation) = false do
        let instruction = instructions[nextInstruction]
        nextInstruction <- (nextInstruction + 1) % instructions.Length
        currentLocation <- if instruction = 'L' then map[currentLocation].Left else map[currentLocation].Right
        steps <- steps + 1

    steps

let GoalReachedPart1(loc:string) =
    loc = "ZZZ"
    
let GoalReachedPart2(loc:string) =
    loc[loc.Length - 1] = 'Z'

let mutable startLocation = map.Keys |> Seq.filter (fun name -> name[name.Length - 1] = 'A') |> Seq.head
printfn "[Part 1] Number of steps to reach ZZZ = %d" (CalculateStepsToReachGoal("AAA", GoalReachedPart1))

let mutable currentLocations = map.Keys |> Seq.filter (fun name -> name[name.Length - 1] = 'A') |> Seq.toArray

let periodicity =
    currentLocations
    |> Seq.map (fun loc -> CalculateStepsToReachGoal(loc, GoalReachedPart2))

periodicity
    |> Seq.iter (fun period -> printfn "%A" period)

printfn "To find the answer, paste the above six values into an online LCM calculator"
printfn "[Part 2: The answer turned out to be 10921547990923"

