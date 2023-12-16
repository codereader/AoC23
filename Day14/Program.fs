open System

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

let TransposeLines(lines: string array) =
    seq { 0 .. lines[0].Length - 1 }
    |> Seq.map (fun col -> 
        let column =
            seq { 0 .. lines.Length - 1 }
            |> Seq.map (fun row -> lines[row][col])
            |> Seq.toArray
        (new string(column)))

let input = TransposeLines(lines) |> Seq.toArray

let TiltLeft(row: string) =
    let chars = row.ToCharArray()
    for idx in { 0..chars.Length-2 } do
        if chars[idx] = '.' then
            let nextRockIndex = chars |> Seq.skip(idx) |> Seq.tryFindIndex (fun ch -> ch = 'O' || ch = '#')
            if nextRockIndex.IsSome && chars[nextRockIndex.Value + idx] = 'O' then
                chars[idx] <- 'O'
                chars[nextRockIndex.Value + idx] <- '.'
    new String(chars)

let tiltedPlatform =
    seq { 0..input.Length-1 }
    |> Seq.map (fun idx -> TiltLeft input[idx])
    |> Seq.toArray

// Consider the weight of all the rocks (index is 1-based!)
let CalculateWeight(line:string) =
    seq { 0..line.Length-1 }
        |> Seq.filter (fun index -> line[index] = 'O')
        |> Seq.map (fun idx -> line.Length - idx)
        |> Seq.sum

let part1Sum = 
    tiltedPlatform
    |> Seq.map CalculateWeight
    |> Seq.sum

// Correct answer is 107430
printfn "[Part 1]: Total Weight after tilting north = %d" part1Sum
