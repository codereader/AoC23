open System

let map = IO.File.ReadAllLines @"..\..\..\input.txt"
let mapWidth = map[0].Length

// returns true if the given index points to the start of a number string in the given line
let IsNumberStart (index, line:string) =
    Char.IsDigit(line[index]) && (index = 0 || Char.IsDigit(line[index - 1]) = false)

// Locates the end index of the number in the given line
let FindNumberEndIndex (startIndex: int, line:string) =
    let indexAfterNumber = line.Substring(startIndex) |> Seq.tryFindIndex (fun ch -> (Char.IsDigit(ch) = false))
    if indexAfterNumber.IsSome then startIndex + indexAfterNumber.Value - 1 else line.Length - 1

// true if the coordinates point to a symbol (indicating the neighbouring number is a part number)
let IsSymbol ((x,y)) =
    let ch = map[y][x]
    Char.IsDigit(ch) = false && ch <> '.'

// True if the given coordinate pair is not out of bounds
let IsValid ((x, y)) =
    x >= 0 && y >= 0 && x < mapWidth && y < map.Length

// Generates a coordinate pair for every position surrounding the given number
let GetSurroundingCoordinates ((startX, endX), y) =
    let fullXRange = seq { startX - 1 .. endX + 1}
    let upperIndices = fullXRange |> Seq.map (fun x -> (x, y - 1))
    let lowerIndices = fullXRange |> Seq.map (fun x -> (x, y + 1))
    let sides = seq { (startX - 1, y); (endX + 1, y) }

    seq { upperIndices; lowerIndices; sides } |> Seq.concat |> Seq.filter (fun pair -> IsValid pair)

// True if there's a symbol at any of the given positions
let FindSymbol ((x,y), lineIndex) =
    let surrounding = GetSurroundingCoordinates ((x,y), lineIndex)
    let foundSymbol = surrounding |> Seq.tryFind IsSymbol
    foundSymbol.IsSome

// Parse the number from the given coordinate range
let ExtractNumber((startX, endX), lineIndex) =
    int (map[lineIndex].Substring(startX, endX - startX + 1))

// Calculate the sum of all part numbers found in the given line
let CalculateSumForLine (lineIndex) = 
    
    let line = map[lineIndex]

    let numberStartIndices = seq { 0 .. line.Length - 1 } |> Seq.filter (fun (i) -> IsNumberStart(i, line))
    let numberIndices = numberStartIndices |> Seq.map (fun index -> (index, FindNumberEndIndex(index, line)))

    let partNumberIndices = numberIndices |> Seq.filter (fun pair -> FindSymbol (pair, lineIndex))

    let lineSum = partNumberIndices |> Seq.map (fun pair -> ExtractNumber(pair, lineIndex)) |> Seq.sum

    lineSum

let total = seq { 0 .. map.Length - 1 } |> Seq.map CalculateSumForLine |> Seq.sum
printfn "[Part 1]: Sum of all part numbers = %d" total

