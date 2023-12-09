open Day8
open System

// Real puzzle input
let lines = IO.File.ReadAllLines @"..\..\..\input.txt"
// test input
(*let lines = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)".Replace("\r", "").Split("\n")*)

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

let mutable steps = 0
let mutable nextInstruction = 0
let mutable currentLocation = "AAA"

while currentLocation <> "ZZZ" do
    let instruction = instructions[nextInstruction]
    nextInstruction <- (nextInstruction + 1) % instructions.Length
    currentLocation <- if instruction = 'L' then map[currentLocation].Left else map[currentLocation].Right
    steps <- steps + 1

printfn "[Part 1] Number of steps to reach ZZZ = %d" steps