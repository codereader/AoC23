open System

let map = IO.File.ReadAllLines @"..\..\..\input.txt"
let mapWidth = map[0].Length

let IsNumberStart (index, line:string) =
    Char.IsDigit(line[index]) && (index = 0 || Char.IsDigit(line[index - 1]) = false)

let FindNumberEndIndex (startIndex: int, line:string) =
    let indexAfterNumber = line.Substring(startIndex) |> Seq.tryFindIndex (fun ch -> (Char.IsDigit(ch) = false))
    if indexAfterNumber.IsSome then startIndex + indexAfterNumber.Value - 1 else line.Length - 1

let IsSymbol ((x,y)) =
    let ch = map[y][x]
    Char.IsDigit(ch) = false && ch <> '.'

let IsValid ((x, y)) =
    x >= 0 && y >= 0 && x < mapWidth && y < map.Length

let GetSurroundingCoordinates ((startX, endX), y) =
    let fullXRange = seq { startX - 1 .. endX + 1}
    let upperIndices = fullXRange |> Seq.map (fun x -> (x, y - 1))
    let lowerIndices = fullXRange |> Seq.map (fun x -> (x, y + 1))
    let sides = seq { (startX - 1, y); (endX + 1, y) }

    seq { upperIndices; lowerIndices; sides } |> Seq.concat |> Seq.filter (fun pair -> IsValid pair)

let FindSymbol ((x,y), lineIndex) =
    let surrounding = GetSurroundingCoordinates ((x,y), lineIndex)
    let foundSymbol = surrounding |> Seq.tryFind IsSymbol
    foundSymbol.IsSome

let ExtractNumber((startX, endX), lineIndex) =
    int (map[lineIndex].Substring(startX, endX - startX + 1))

let CalculateSumForLine (lineIndex) = 
    
    let line = map[lineIndex]

    let numberStartIndices = seq { 0 .. line.Length - 1 } |> Seq.filter (fun (i) -> IsNumberStart(i, line))
    let numberIndices = numberStartIndices |> Seq.map (fun index -> (index, FindNumberEndIndex(index, line)))

    let partNumberIndices = numberIndices |> Seq.filter (fun pair -> FindSymbol (pair, lineIndex))

    //printfn "%s" line
    //printfn "%A" partNumberIndices

    let lineSum = partNumberIndices |> Seq.map (fun pair -> ExtractNumber(pair, lineIndex)) |> Seq.sum

    //printfn "Sum for this line = %d" lineSum

    lineSum

let total = seq { 0 .. map.Length - 1 } |> Seq.map CalculateSumForLine |> Seq.sum
printfn "[Part 1]: Sum of all part numbers = %d" total