open System
open System.Collections.Generic

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
// Test input
lines <- "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....".Replace("\r\n", "\n").Split('\n')
*)

let TurnPlatformRight(lines: string array) =
    let N = lines.Length
    seq {
        for y in { 0..N-1 } do
            let newLine = Array.zeroCreate<char>(N)
            for x in { 0..N-1 } do
                newLine[x] <- lines[N-1-x][y]
            yield new String(newLine)
    }
    |> Seq.toArray

// Turn left 90 degrees, then let them roll
let input = TurnPlatformRight(TurnPlatformRight(TurnPlatformRight(lines)))

let TiltLeft(row: string) =
    let chars = row.ToCharArray()
    for idx in { 0..chars.Length-2 } do
        if chars[idx] = '.' then
            let nextRockIndex = chars |> Seq.skip(idx) |> Seq.tryFindIndex (fun ch -> ch = 'O' || ch = '#')
            if nextRockIndex.IsSome && chars[nextRockIndex.Value + idx] = 'O' then
                chars[idx] <- 'O'
                chars[nextRockIndex.Value + idx] <- '.'
    new String(chars)

let TiltPlatform(input:string array) =
    seq { 0..input.Length-1 }
    |> Seq.map (fun idx -> TiltLeft input[idx])
    |> Seq.toArray

// Consider the weight of all the rocks (index is 1-based!)
let CalculateWeightPart1(line:string) =
    seq { 0..line.Length-1 }
        |> Seq.filter (fun index -> line[index] = 'O')
        |> Seq.map (fun idx -> line.Length - idx)
        |> Seq.sum

let GetNumberOfStones(line:string) =
    line |> Seq.filter (fun ch -> ch = 'O') |> Seq.length

let CalculatePlatformWeight(platform: string array) =
    platform
    |> Seq.mapi (fun i line -> (GetNumberOfStones line)*(platform.Length - i))
    |> Seq.sum

let part1Sum = 
    TiltPlatform(input)
    |> Seq.map CalculateWeightPart1
    |> Seq.sum

// Correct answer is 107430
printfn "[Part 1]: Total Weight after tilting north = %d" part1Sum

// Part 2 Run 1 billion cycles

let CalculateSituationHash(platform:string array) =
    let mutable hash = 23
    platform
        |> Seq.map (fun line -> line.GetHashCode())
        |> Seq.iter (fun h -> hash <- hash * 31 + h)
    hash

let PrintPlatform(input) =
    input |> Seq.iter (fun line -> printfn "%s" line)

// Tilt North, Wast, South, East

let RunCycle(input) =
    // Start north
    let mutable platform = TurnPlatformRight(TurnPlatformRight(TurnPlatformRight(input)))
    platform <- TiltPlatform(platform) // North

    platform <- TurnPlatformRight(platform)

    //printfn "-----\nAfter N\n-----"
    //PrintPlatform platform
    platform <- TiltPlatform(platform) // West

    //printfn "-----\nAfter W\n-----"
    //PrintPlatform platform

    platform <- TurnPlatformRight(platform)
    platform <- TiltPlatform(platform) // South

    //printfn "-----\nAfter S\n-----"
    //PrintPlatform (TurnPlatformRight(TurnPlatformRight(TurnPlatformRight(platform))))

    platform <- TurnPlatformRight(platform)
    platform <- TiltPlatform(platform) // East
    TurnPlatformRight(TurnPlatformRight(platform)) // turn into starting orientation

let mutable cycleHashes = new Dictionary<int, int>()
let mutable situations = new List<string[]>()

let mutable platform = lines
let mutable cycleCount = 0
situations.Add(platform)

while cycleCount >= 0 do
    platform <- RunCycle platform
    cycleCount <- cycleCount + 1
    let hash = CalculateSituationHash platform
    //printfn "-----\nAfter %d cycles, Hash = %d\n-----" cycleCount hash

    let hashCollision, prevCycleCount = cycleHashes.TryGetValue hash
    if hashCollision then
        printfn "Got a repeating pattern: First occurrence in cycle %d, now in %d" prevCycleCount cycleCount
        let offset = prevCycleCount
        let period = cycleCount - prevCycleCount

        let offsetWithinPeriod = (1_000_000_000 - offset) % period

        // Look up this situation, we've had it before
        let situationIndex = offset + offsetWithinPeriod
        let platformAfter1BillionCycles = situations[situationIndex]
        printfn "[Part 2]: Total Weight after 1B cycles = %d" (CalculatePlatformWeight(platformAfter1BillionCycles))
        cycleCount <- -1 // break
        //cycleHashes.Clear()
    else
        cycleHashes[hash] <- cycleCount
        situations.Add(platform)
        //printfn "Weight after %d cycles = %d" cycleCount (CalculatePlatformWeight platform)
       
    //if cycleCount >= 0 then
    //    PrintPlatform(situations[cycleCount])
