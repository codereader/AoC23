open System
open Utils

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

let originalWidth = lines[0].Length
let originalHeight = lines.Length

// Find columns without any galaxies
let columnsWithoutGalaxies = 
    seq { 0..originalWidth-1 }
    |> Seq.filter (fun col ->
        seq { 0..originalWidth-1 } |> Seq.forall (fun row -> lines[row][col] = '.')
    )
    |> Seq.toArray

let emptyLine = new String('.', originalWidth)

let rowsWithoutGalaxies =
    seq {
    for row in { 0..lines.Length-1 } do
        if lines[row] |> Seq.forall (fun ch -> ch = '.') then
            yield row }
    |> Seq.toArray

printfn "Columns without galaxies: %A" columnsWithoutGalaxies
printfn "Rows without galaxies: %A" rowsWithoutGalaxies

let galaxies =
    seq {
        for row in { 0.. originalHeight-1 } do
            for col in { 0.. originalWidth-1 } do
                if lines[row][col] = '#' then
                    yield (col,row) }
    |> Seq.map Vector2
    |> Seq.toList

let galaxyPairs =
    seq {
    for i in { 0..galaxies.Length-1 } do
        for j in { i+1..galaxies.Length-1 } do
            yield (galaxies[i], galaxies[j]) }
    |> Seq.toArray

printfn "%d galaxy pairs" galaxyPairs.Length

type Range(a:int, b:int) =
    member this.Min = System.Math.Min(a, b)
    member this.Max = System.Math.Max(a, b)
    member this.Distance = System.Math.Abs(a - b)
    member this.Contains(value) = this.Min <= value && this.Max >= value

let CalculateDistance((galaxy1: Vector2, galaxy2: Vector2), spacing: int) =
    let rangeX = Range(galaxy1.X, galaxy2.X)
    let rangeY = Range(galaxy1.Y, galaxy2.Y)

    let xCrossings = 
        columnsWithoutGalaxies
        |> Seq.filter (fun col -> rangeX.Contains col)
        |> Seq.length

    let yCrossings = 
        rowsWithoutGalaxies
        |> Seq.filter (fun row -> rangeY.Contains row)
        |> Seq.length

    rangeX.Distance + rangeY.Distance + xCrossings * spacing + yCrossings * spacing

let totalDistancePart1 = 
    galaxyPairs
    |> Seq.sumBy (fun pair -> CalculateDistance(pair, 1))

printfn "[Part 1]: Total Distance = %d" totalDistancePart1

let totalDistancePart2 = 
    galaxyPairs
    |> Seq.map (fun pair -> CalculateDistance(pair, 999_999))
    |> Seq.sumBy (fun dist -> uint64 dist)

printfn "[Part 2]: Total Distance = %d" totalDistancePart2
