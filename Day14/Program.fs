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

let TurnPlatformLeft(lines) = TurnPlatformRight(TurnPlatformRight(TurnPlatformRight(lines)))

// Tilt the platform to let the stones roll left
// We're tilting left to make use of the left-bound FindIndex and tryFindIndex and new String() methods
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

let GetNumberOfStones(line:string) =
    line |> Seq.filter (fun ch -> ch = 'O') |> Seq.length

// Consider the weight of all the rocks (first row gets weight N per stone)
let CalculatePlatformWeight(platform: string array) =
    platform
    |> Seq.mapi (fun i line -> (GetNumberOfStones line)*(platform.Length - i))
    |> Seq.sum

// Turn left 90 degrees, then let them roll, roll back and calculate the weight
let part1Sum = lines |> TurnPlatformLeft |> TiltPlatform |> TurnPlatformRight |> CalculatePlatformWeight

// Correct answer is 107430
printfn "[Part 1]: Total Weight after tilting north = %d" part1Sum

// Part 2 Run 1 billion NWSE tilt cycles

let CalculateSituationHash(platform:string array) =
    let mutable hash = 23
    platform
        |> Seq.map (fun line -> line.GetHashCode())
        |> Seq.iter (fun h -> hash <- hash * 31 + h)
    hash

let PrintPlatform(input) =
    input |> Seq.iter (fun line -> printfn "%s" line)

// Tilt North, Wast, South, East, return the new result
let RunCycle(input) =
    // Start tilting north, so turn the table to the left to boot
    let mutable platform = TurnPlatformLeft(input)
    platform <- TiltPlatform(platform) // North

    platform <- TurnPlatformRight(platform) // turn right to tilt West
    platform <- TiltPlatform(platform)

    platform <- TurnPlatformRight(platform) // turn right to tilt South
    platform <- TiltPlatform(platform)

    platform <- TurnPlatformRight(platform) // turn right to tilt East
    platform <- TiltPlatform(platform)

    TurnPlatformRight(TurnPlatformRight(platform)) // turn 180 deg back into starting orientation

// Keep track of all situations we encounter until we complete the first cycle
let mutable cycleHashes = new Dictionary<int, int>()
let mutable situations = new List<string array>()

let mutable platform = lines // start from the initial puzzle input
let mutable cycleCount = 0
situations.Add(platform)

while cycleCount >= 0 do
    // Run a cycle then store the result
    platform <- RunCycle platform
    cycleCount <- cycleCount + 1
    situations.Add(platform)

    // Get the hash to see if we encountered this situation before
    let hash = CalculateSituationHash platform

    let isKnownSituation, prevCycleCount = cycleHashes.TryGetValue hash

    if isKnownSituation then
        printfn "Got a repeating pattern: First occurrence in cycle %d, now in %d" prevCycleCount cycleCount
        let offset = prevCycleCount
        let period = cycleCount - prevCycleCount

        // Map the 1B cycles into our first cycle range
        let offsetWithinPeriod = (1_000_000_000 - offset) % period

        // Look up this situation, we've recorded it before
        let situationIndex = offset + offsetWithinPeriod
        let platformAfter1BillionCycles = situations[situationIndex]

        // Correct answer is 96317
        printfn "[Part 2]: Total Weight after 1B cycles = %d" (CalculatePlatformWeight(platformAfter1BillionCycles))
        cycleCount <- -1 // break
    else
        cycleHashes[hash] <- cycleCount

    //if cycleCount >= 0 then
    //    PrintPlatform(situations[cycleCount])
