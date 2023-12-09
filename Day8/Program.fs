open Day8
open System

// Real puzzle input
//let lines = IO.File.ReadAllLines @"..\..\..\input.txt"
// test input
let lines = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)".Replace("\r", "").Split("\n")

let instructions = lines[0]

let mapLines =
    lines
    |> Seq.skip 2

let map =
    mapLines
    |> Seq.map Location
    |> Seq.map (fun location -> location.Name, location)
    |> dict

printfn "%A" map

let RunPart1 = 
    let mutable steps = 0
    let mutable nextInstruction = 0
    let mutable currentLocation = "AAA"

    while currentLocation <> "ZZZ" do
        let instruction = instructions[nextInstruction]
        nextInstruction <- (nextInstruction + 1) % instructions.Length
        currentLocation <- if instruction = 'L' then map[currentLocation].Left else map[currentLocation].Right
        steps <- steps + 1

    steps

//printfn "[Part 1] Number of steps to reach ZZZ = %d" RunPart1

let mutable steps = 0L
let mutable nextInstruction = 0
let mutable currentLocations = map.Keys |> Seq.filter (fun name -> name[name.Length - 1] = 'A') |> Seq.toArray

let GoalReached =
    currentLocations |> Seq.exists (fun name -> name[name.Length - 1] <> 'Z') = false

while (not GoalReached) do
    let instruction = instructions[nextInstruction]
    nextInstruction <- (nextInstruction + 1) % instructions.Length
    for i in { 0..currentLocations.Length - 1 } do
        currentLocations[i] <- if instruction = 'L' then map[currentLocations[i]].Left else map[currentLocations[i]].Right
    steps <- steps + 1L
    if steps % 10000000L = 0L then
        printfn "%d steps reached" steps

printfn "[Part 2] Number of steps to reach ZZZ = %d" steps

