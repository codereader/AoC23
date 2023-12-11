open System

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

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

let originalWidth = lines[0].Length

// Find columns without any galaxies
let columnsWithoutGalaxies = 
    seq { 0..originalWidth-1 }
    |> Seq.filter (fun col ->
        seq { 0..originalWidth-1 } |> Seq.forall (fun row -> lines[row][col] = '.')
    )
    |> Seq.toArray

let rowsWithoutGalaxies =
    seq { 0..lines.Length-1 }
    |> Seq.filter (fun row ->
        lines[row] |> Seq.forall (fun ch -> ch = '.')
    )
    |> Seq.toArray

let emptyLine = new String('.', originalWidth)

let spacedRows = seq {
    for row in { 0..lines.Length-1 } do
        if rowsWithoutGalaxies |> Array.contains row then
            yield emptyLine
        yield lines[row]
    }

//let mutable spacedInput = []

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

