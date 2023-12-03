open System

let map = IO.File.ReadAllLines @"..\..\..\input.txt"
let mapWidth = map[0].Length

// True if the given coordinate pair is not out of bounds
let IsValid ((x, y)) =
    x >= 0 && y >= 0 && x < mapWidth && y < map.Length

// true if the coordinates point to a symbol (indicating the neighbouring number is a part number)
let IsSymbol ((x,y)) =
    let ch = map[y][x]
    Char.IsDigit(ch) = false && ch <> '.'

type Number(startX, endX, lineIndex) =
    member val StartX = startX
    member val EndX = endX
    member val LineIndex = lineIndex

    member this.NumberValue = int (map[this.LineIndex].Substring(this.StartX, this.EndX - this.StartX + 1))

    // Generates a coordinate pair for every position surrounding the given number
    member private this.GetSurroundingCoordinates ((startX, endX), y) =
        let fullXRange = seq { startX - 1 .. endX + 1}
        let upperIndices = fullXRange |> Seq.map (fun x -> (x, y - 1))
        let lowerIndices = fullXRange |> Seq.map (fun x -> (x, y + 1))
        let sides = seq { (startX - 1, y); (endX + 1, y) }

        seq { upperIndices; lowerIndices; sides } |> Seq.concat |> Seq.filter (fun pair -> IsValid pair)

    member this.IsPartNumber =
        let surrounding = this.GetSurroundingCoordinates ((startX,endX), lineIndex)
        let foundSymbol = surrounding |> Seq.tryFind IsSymbol
        foundSymbol.IsSome

    member private this.TryFindGearPosition =
        let surrounding = this.GetSurroundingCoordinates ((startX,endX), lineIndex)
        surrounding |> Seq.tryFind (fun (x,y) -> map[y][x] = '*')

    member this.IsGear = 
        this.TryFindGearPosition.IsSome

    member this.GearPosition =
        this.TryFindGearPosition.Value

// returns true if the given index points to the start of a number string in the given line
let IsNumberStart (index, line:string) =
    Char.IsDigit(line[index]) && (index = 0 || Char.IsDigit(line[index - 1]) = false)

// Locates the end index of the number in the given line
let FindNumberEndIndex (startIndex: int, line:string) =
    let indexAfterNumber = line.Substring(startIndex) |> Seq.tryFindIndex (fun ch -> (Char.IsDigit(ch) = false))
    if indexAfterNumber.IsSome then startIndex + indexAfterNumber.Value - 1 else line.Length - 1

// Find all numbers in the given line
let FindNumbersInLine (lineIndex) =
    let line = map[lineIndex]

    // Find all start indices
    let numberStartIndices = seq { 0 .. line.Length - 1 } |> Seq.filter (fun (i) -> IsNumberStart(i, line))
    // Find the corresponding end index
    let numberIndices = numberStartIndices |> Seq.map (fun index -> (index, FindNumberEndIndex(index, line)))

    // Take the number pairs and construct Number objects
    numberIndices |> Seq.map (fun (startX, endX) -> Number(startX, endX, lineIndex))

let FindNumbers =
    seq { 0 .. map.Length - 1 } |> Seq.collect FindNumbersInLine

let FindPartNumbers =
    FindNumbers |> Seq.filter (fun number -> number.IsPartNumber)

// Calculate the sum of all part numbers
let CalculatePartNumberSum = 
    FindPartNumbers |> Seq.map (fun part -> part.NumberValue) |> Seq.sum

// Calculate the sum of all gear ratios
let CalculateGearRatio =
    // All part number indices of all lines
    let allGears = FindPartNumbers |> Seq.filter (fun number -> number.IsGear)

    // Group all gear part numbers by the gear position, but only take those with 2 gears
    let gearGroups = allGears |> Seq.groupBy (fun gear -> gear.GearPosition) |> Seq.filter (fun (_, gears) -> (Seq.length gears) = 2)

    gearGroups |> Seq.map (fun (_, gears) -> ((Seq.head gears).NumberValue * (Seq.last gears).NumberValue)) |> Seq.sum

let total =  CalculatePartNumberSum
printfn "[Part 1]: Sum of all part numbers = %d" total

let totaGearRatio = CalculateGearRatio
printfn "[Part 2]: Sum of all gear ratios = %d" totaGearRatio