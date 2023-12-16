open System

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

let RowToInt(line:string) =
    Convert.ToInt32(line.Replace("#", "1").Replace(".", "0"), 2)

let PrintLine(line:int) =
    let str = Convert.ToString(line, 2)
    let padding = new String('0', 18 - str.Length)
    printfn "%s%s" padding str

let PrintBlock(lines:int array) =
    let strings = 
        seq { 0..lines.Length-1 }
        |> Seq.map (fun i -> Convert.ToString(lines[i], 2))
        |> Seq.toArray
    let maxLength = strings |> Seq.map (fun s -> s.Length) |> Seq.max
    strings
        |> Seq.iter (fun str -> 
            let padding = new String('0', maxLength - str.Length)
            printfn "%s%s" padding str
        )

let PrintStringBlock(lines:string array) =
    lines |> Seq.iter (fun line -> printfn "%s" line)

let CalculateRowHashes(block) = 
    block
    |> Seq.map RowToInt
    |> Seq.toArray

let ColumnsofBlock(block: string array) =
    seq { 0 .. block[0].Length - 1 }
    |> Seq.map (fun col -> 
        let column =
            seq { 0 .. block.Length - 1 }
            |> Seq.map (fun row -> block[row][col])
            |> Seq.toArray
        (new String(column)))

let CalculateColumnHashes(block:string array) =
    ColumnsofBlock(block)
        |> Seq.map RowToInt
        |> Seq.toArray

let ClassifyMirrorIndex(candidate: int, hashes: int array) =
    let forward = seq { candidate+1 .. hashes.Length-1 }
    let backward = seq { candidate .. -1 .. 0 }
    let mutable mismatchingIndices = 0
    let mutable matchingIndices = 0
    Seq.iter2 (fun idx1 idx2 ->
        if hashes[idx1] <> hashes[idx2] then
            mismatchingIndices <- mismatchingIndices + 1
        else
            matchingIndices <- matchingIndices + 1
        ) forward backward
    (matchingIndices, mismatchingIndices)

let ClassifyMirrorIndices(hashes: int array) =
    seq { 0 .. hashes.Length - 2 } |> Seq.map (fun i -> (i, ClassifyMirrorIndex(i, hashes)))

let FindMirrorIndices(hashes: int array) =
    seq { 0 .. hashes.Length - 2 } |> Seq.filter (fun i -> hashes[i] = hashes[i+1] && snd (ClassifyMirrorIndex(i, hashes)) = 0)

let TryCalculateBlockValue(rowHashes, columnHashes, rowToAvoid: int option, colToAvoid: int option) =
    let rowIndex = FindMirrorIndices(rowHashes) |> Seq.filter (fun idx -> rowToAvoid.IsNone || idx <> rowToAvoid.Value) |> Seq.tryHead

    if rowIndex.IsSome then
        (rowIndex, None, Some((rowIndex.Value + 1) * 100))
    else
        let colIndex = FindMirrorIndices(columnHashes) |> Seq.filter (fun idx -> colToAvoid.IsNone || idx <> colToAvoid.Value) |> Seq.tryHead
        if colIndex.IsSome then (None, colIndex, Some(colIndex.Value + 1)) else (None, None, None)

let FlipChar(ch) = if ch = '#' then '.' else '#'

let FlipCharInLine(line:string, x) =
    new String(line
        |> Seq.mapi (fun idx ch -> if idx = x then FlipChar(ch) else ch)
        |> Seq.toArray)

let FlipCharInBlock(block: string array, x, y) = 
    seq { 0 .. block.Length-1 }
        |> Seq.map (fun row -> if y = row then FlipCharInLine(block[row], x) else block[row])
        |> Seq.toArray

let CalculateBlockSumPart2(block: string array) =
    
    let rowHashes = CalculateRowHashes(block)
    let columnHashes = CalculateColumnHashes(block)

    let (origRow, origCol, originalValue) = TryCalculateBlockValue(rowHashes, columnHashes, None, None)

    let blockWidth = block[0].Length
    let blockHeight = block.Length

    //printfn "Original Block has value %A" originalValue
    //PrintStringBlock block

    let changedBlockValues = seq {
        for y in seq { 0 .. blockHeight-1 } do
            for x in seq { 0 .. blockWidth-1 } do
                let alternativeBlock = FlipCharInBlock(block, x, y)
                let altRowHashes = CalculateRowHashes(alternativeBlock)
                let altColumnHashes = CalculateColumnHashes(alternativeBlock)
                let (_, _, altValue) = TryCalculateBlockValue(altRowHashes, altColumnHashes, origRow, origCol)

                if altValue.IsSome then
                    yield altValue.Value
    }

    let uniqueValues = 
        changedBlockValues
        |> Seq.distinct
        |> Seq.toArray

    //printfn "Alternative Blocks found with values %s" (String.Join("\n", uniqueValues))
    uniqueValues[0]

let blockSumPart1 =
    blocks
    |> Seq.map (fun block ->
        let rowHashes = CalculateRowHashes(block)
        let columnHashes = CalculateColumnHashes(block)

        let (_, _, value) = TryCalculateBlockValue(rowHashes, columnHashes, None, None)
        value
        )
    |> Seq.sumBy (fun i -> i.Value)

// Valid answer: 33047
printfn "[Part 1]: Block sum = %d" blockSumPart1

let blockSumPart2 =
    blocks
    |> Seq.map CalculateBlockSumPart2
    |> Seq.sum

// Valid answer: 28806
printfn "[Part 2]: Block sum = %d" blockSumPart2
