open System
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
// Test input
lines <- "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#".Replace("\r\n", "\n").Split('\n')
*)

let emptyLineIndices =
    seq { 0.. lines.Length-1 }
    |> Seq.filter (fun lineIndex -> String.IsNullOrEmpty(lines[lineIndex]))

let blocks = 
    Seq.append (seq { -1; }) (Seq.append emptyLineIndices (seq { lines.Length }))
    |> Seq.pairwise
    |> Seq.map (fun (startIndex, endIndex) -> (lines |> Seq.skip (startIndex + 1) |> Seq.take (endIndex - startIndex - 1) |> Seq.toArray ))
    |> Seq.toArray

printfn "Blocks: %d" blocks.Length

let CalculateBlockSum(block: string array) =
    let rowHashes = 
        block
        |> Seq.map (fun line -> line.GetHashCode())
        |> Seq.toArray

    let columnHashes =
        seq { 0 .. block[0].Length - 1 }
        |> Seq.map (fun col -> 
            let column =
                seq { 0 .. block.Length - 1 }
                |> Seq.map (fun row -> block[row][col])
                |> Seq.toArray
            (new String(column)).GetHashCode()
        )
        |> Seq.toArray

    let ValidateMirrorIndex(candidate: int, hashes: int array) =
        let forward = seq { candidate+1 .. hashes.Length-1 }
        let backward = seq { candidate .. -1 .. 0 }
        Seq.forall2 (fun idx1 idx2 -> hashes[idx1] = hashes[idx2]) forward backward

    let FindMirrorIndices(hashes: int array) =
        seq { 0 .. hashes.Length - 2 } |> Seq.filter (fun i -> hashes[i] = hashes[i+1] && ValidateMirrorIndex(i, hashes))

    let rowIndices = FindMirrorIndices rowHashes |> Seq.toArray

    if rowIndices.Length > 0 then
        (rowIndices[0] + 1) * 100
    else
        let colIndices = FindMirrorIndices columnHashes
        (Seq.head colIndices) + 1

let blockSum = blocks |> Seq.map CalculateBlockSum |> Seq.sum

printfn "[Part 1]: Block sum = %d" blockSum
