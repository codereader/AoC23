open System
open Utils
open System.Collections.Generic

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
// Test input
lines <- @".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....".Replace("\r\n", "\n").Split('\n')
*)

let Left = Vector2.Left
let Right = Vector2.Right
let Up = Vector2.Up
let Down = Vector2.Down

type Beam = { Position: Vector2; Direction: Vector2 }
type Cell = { mutable IsIlluminated: bool; CellType: char; mutable BeamDirections: HashSet<Vector2> }

let gridSize = Vector2(lines[0].Length, lines.Length)

let GenerateStartingGrid() =
    seq { 0..lines.Length-1 } 
    |> Seq.collect (fun y -> seq {
            for x in 0..lines[y].Length-1 do
                yield (Vector2(x,y), { IsIlluminated = false; CellType = lines[y][x]; BeamDirections = new HashSet<Vector2>() })
        })
    |> dict

let PositionIsWithinGrid(position: Vector2) = position.X >= 0 && position.X < gridSize.X && position.Y >= 0 && position.Y < gridSize.Y

let PositionHasEquivalentBeam(grid: IDictionary<Vector2, Cell>, position, direction) =
    grid[position].BeamDirections.Contains(direction)

type Interaction =
    | Proceed = 0
    | SplitUpDown = 1
    | SplitLeftRight = 2
    | DeflectUp = 3
    | DeflectRight = 4
    | DeflectLeft = 5
    | DeflectDown = 6
    | Stop = 7

let GetInteraction(grid: IDictionary<Vector2, Cell>, position, direction) =
    if PositionIsWithinGrid position then
        match grid[position].CellType with
        | '.' -> if PositionHasEquivalentBeam(grid, position, direction) then Interaction.Stop else Interaction.Proceed
        | '-' when direction = Left || direction = Right -> Interaction.Proceed
        | '|' when direction = Up || direction = Down -> Interaction.Proceed
        | '\\' when direction = Right -> Interaction.DeflectDown
        | '/' when direction = Left -> Interaction.DeflectDown
        | '/' when direction = Right -> Interaction.DeflectUp
        | '/' when direction = Up -> Interaction.DeflectRight
        | '/' when direction = Down -> Interaction.DeflectLeft
        | '\\' when direction = Left -> Interaction.DeflectUp
        | '\\' when direction = Down -> Interaction.DeflectRight
        | '\\' when direction = Up -> Interaction.DeflectLeft
        | '|' when direction = Left || direction = Right -> Interaction.SplitUpDown
        | '-' when direction = Up || direction = Down -> Interaction.SplitLeftRight
        | _ -> failwith "Unknown interaction type"
    else
        Interaction.Stop // outside grid

let MoveForward(beam: Beam, grid: IDictionary<Vector2, Cell>) =
    let forwardPoints = seq {
        let mutable current = beam.Position
        while true do
            current <- current + beam.Direction
            yield current
    }

    // Find the first interaction and return the pair (Position, Interaction)
    forwardPoints
        |> Seq.map (fun pos -> (pos, GetInteraction(grid, pos, beam.Direction)))
        |> Seq.find (fun (_, interaction) -> interaction <> Interaction.Proceed)

let IlluminateCells(beam, endPoint, grid: IDictionary<Vector2, Cell>) =
    let mutable current = beam.Position + beam.Direction

    while current <> endPoint do
        ignore (grid[current].BeamDirections.Add(beam.Direction))
        grid[current].IsIlluminated <- true
        current <- current + beam.Direction

    // Illuminate the reached position if it is still within the grid
    if PositionIsWithinGrid current then
        grid[current].IsIlluminated <- true

let Cap(value) = if value > 9 then 'A' else (if value = 0 then '.' else value.ToString()[0])

let PrintCell(cell) = 
    if cell.IsIlluminated then
        Console.ForegroundColor <- ConsoleColor.White
    else
        Console.ForegroundColor <- ConsoleColor.DarkGray
        
    let ch = (if cell.CellType = '.' then Cap(cell.BeamDirections.Count) else cell.CellType)
    printf "%c" ch

let PrintGrid(grid: IDictionary<Vector2, Cell>) = 

    let oldColour = Console.ForegroundColor

    for y in { 0..gridSize.Y-1 } do
        for x in { 0..gridSize.X-1 } do
            let cell = grid[Vector2(x,y)]
            PrintCell cell
        printfn ""

    Console.ForegroundColor <- oldColour
    printfn "-----"

let CalculateIlluminationForStartingPosition(grid: IDictionary<Vector2, Cell>, startPosition, startDirection) =
    let mutable unprocessedLightBeams = new Stack<Beam>()

    unprocessedLightBeams.Push({ Position = startPosition; Direction = startDirection })

    //PrintGrid grid

    while unprocessedLightBeams.Count > 0 do
        let beam = unprocessedLightBeams.Pop()

        let target = MoveForward(beam, grid)

        IlluminateCells(beam, fst target, grid)

        //PrintGrid grid

        match snd target with
        | Interaction.DeflectUp -> unprocessedLightBeams.Push({ Position = fst target; Direction = Up })
        | Interaction.DeflectDown -> unprocessedLightBeams.Push({ Position = fst target; Direction = Down })
        | Interaction.DeflectRight -> unprocessedLightBeams.Push({ Position = fst target; Direction = Right })
        | Interaction.DeflectLeft -> unprocessedLightBeams.Push({ Position = fst target; Direction = Left })
        | Interaction.SplitUpDown ->
            unprocessedLightBeams.Push({ Position = fst target; Direction = Up })
            unprocessedLightBeams.Push({ Position = fst target; Direction = Down })
        | Interaction.SplitLeftRight ->
            unprocessedLightBeams.Push({ Position = fst target; Direction = Right })
            unprocessedLightBeams.Push({ Position = fst target; Direction = Left })
        | _ -> ()

    //PrintGrid grid

    grid
        |> Seq.filter (fun cell -> cell.Value.IsIlluminated)
        |> Seq.length

// Correct answer is 7210
let illuminatedCellsPart1 = CalculateIlluminationForStartingPosition(GenerateStartingGrid(), Vector2(-1, 0), Right)
printfn "[Part 1]: Illuminated Cells = %d" illuminatedCellsPart1

let illumnations = seq {
        for y in { 0..gridSize.Y-1 } do
            yield CalculateIlluminationForStartingPosition(GenerateStartingGrid(), Vector2(-1, y), Right)
            yield CalculateIlluminationForStartingPosition(GenerateStartingGrid(), Vector2(gridSize.X, y), Left)

        for x in { 0..gridSize.X-1 } do
            yield CalculateIlluminationForStartingPosition(GenerateStartingGrid(), Vector2(x, -1), Down)
            yield CalculateIlluminationForStartingPosition(GenerateStartingGrid(), Vector2(x, gridSize.Y), Up)
    }

// Correct answer is 7673
printfn "[Part 2]: Maximum Illumnation = %d" (illumnations |> Seq.max)
