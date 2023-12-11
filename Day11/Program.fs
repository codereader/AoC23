open System
open Utils

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"
(*
// Test input
lines <- "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....".Replace("\r\n", "\n").Split('\n')
*)
let originalWidth = lines[0].Length

// Find columns without any galaxies
let columnsWithoutGalaxies = 
    seq { 0..originalWidth-1 }
    |> Seq.filter (fun col ->
        seq { 0..originalWidth-1 } |> Seq.forall (fun row -> lines[row][col] = '.')
    )
    |> Seq.toArray

let mutable rowsWithoutGalaxies = []

let emptyLine = new String('.', originalWidth)

let spacedRows = seq {
    for row in { 0..lines.Length-1 } do
        if lines[row] |> Seq.forall (fun ch -> ch = '.') then
            rowsWithoutGalaxies <- rowsWithoutGalaxies @ [row]
            yield emptyLine
        yield lines[row]
    }

let spacedInput = 
    spacedRows
    |> Seq.map (fun line ->
        let newLineChars =
            seq {
            for col in { 0..originalWidth-1 } do
                if columnsWithoutGalaxies |> Array.contains col then
                    yield '.'
                yield line[col]
            }
            |> Seq.toArray
        new String(newLineChars)
    )
    |> Seq.toList

printfn "Columns without galaxies: %A" columnsWithoutGalaxies
printfn "Rows without galaxies: %A" rowsWithoutGalaxies

spacedInput |> Seq.iter (fun line -> printfn "%s" line)

// Find galaxy positions
let width = spacedInput[0].Length
let height = spacedInput.Length

let galaxies =
    seq {
        for row in { 0.. height-1 } do
            for col in { 0.. width-1 } do
                if spacedInput[row][col] = '#' then
                    yield (col,row) }
    |> Seq.map Vector2
    |> Seq.toList

galaxies |> Seq.iter (fun pos -> printfn "%A" pos)

let galaxyPairs =
    seq {
    for i in { 0..galaxies.Length-1 } do
        for j in { i+1..galaxies.Length-1 } do
            yield (galaxies[i], galaxies[j]) }
    |> Seq.toArray

printfn "%d pairs" galaxyPairs.Length
//galaxyPairs |> Seq.iter (fun (pos1, pos2) -> printfn "%A %A" pos1 pos2)

let CalculateDistance((galaxy1: Vector2, galaxy2: Vector2)) =
    System.Math.Abs(galaxy1.X - galaxy2.X) + System.Math.Abs(galaxy1.Y - galaxy2.Y)

let totalDistanceSum = 
    galaxyPairs
    |> Seq.sumBy CalculateDistance

printfn "[Part 1]: Total Distance = %d" totalDistanceSum