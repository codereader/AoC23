open System
open Utils
open System.Collections.Generic

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
// Test input
lines <- @"2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533".Replace("\r\n", "\n").Split('\n')
*)

type Cell = { mutable MinimumHeatLoss: int; CostToEnter: int }

let gridSize = Vector2(lines[0].Length, lines.Length)

let GenerateGrid() =
    seq { 0..lines.Length-1 } 
    |> Seq.collect (fun y -> seq {
            for x in 0..lines[y].Length-1 do
                yield (Vector2(x,y), { MinimumHeatLoss = Int32.MaxValue; CostToEnter = Int32.Parse((lines[y][x]).ToString()) })
        })
    |> dict

let PositionIsWithinGrid(position: Vector2) = position.X >= 0 && position.X < gridSize.X && position.Y >= 0 && position.Y < gridSize.Y

let grid = GenerateGrid()

type Path(grid: IDictionary<Vector2, Cell>, steps: List<Vector2>) =
    let _heatLoss = steps |> Seq.skip(1) |> Seq.map (fun pos -> grid[pos].CostToEnter) |> Seq.sum

    member this.Grid = grid
    member this.Steps = steps
    member this.Position = this.Steps[this.Steps.Count - 1]
    member this.LastDirection = 
        if this.Steps.Count > 1 then
            this.Position - this.Steps[this.Steps.Count - 2]
        else
            Vector2.Right

    member this.Right = this.Position + this.RightDirection
    member this.Left = this.Position - this.RightDirection
    member this.Forward = this.Position + this.LastDirection

    member this.RightDirection =
        let lastDirection = this.LastDirection
        Vector2(lastDirection.Y, lastDirection.X * -1)

    member this.CanMoveForward =
        let pathLength = this.Steps.Count
        if pathLength < 4 then
            true
        else
            let distance = this.Steps[pathLength - 4] - this.Steps[pathLength - 1]
            if Math.Abs(distance.X) = 3 || Math.Abs(distance.Y) = 3 then false else true

    member this.HeatLoss = _heatLoss

    new(grid) = Path(grid, new List<Vector2>())
    new(grid, startPosition:Vector2) =
        let steps = new List<Vector2>()
        steps.Add(startPosition)
        Path(grid, steps)

    member this.Append(position: Vector2) =
        let steps = new List<Vector2>(this.Steps)
        steps.Add(position)
        Path(grid, steps)

    override this.ToString() =
        sprintf "%s" (String.Join(" > ", this.Steps |> Seq.map(fun step -> step.ToString())))

let PrintPath(grid: IDictionary<Vector2, Cell>, path: Path) = 
    let oldColour = Console.ForegroundColor

    for y in { 0..gridSize.Y-1 } do
        for x in { 0..gridSize.X-1 } do
            let pos = Vector2(x,y)
            let cell = grid[pos]
            if path.Steps |> Seq.exists (fun step -> step = pos) then
                Console.ForegroundColor <- ConsoleColor.White
            else
                Console.ForegroundColor <- ConsoleColor.DarkGray

            printf "%d" cell.CostToEnter
        printfn ""

    Console.ForegroundColor <- oldColour
    printfn "-----"

let mutable pathsToInvestigate = new Queue<Path>()
let startPosition = Vector2(0,0)
let endPosition = Vector2(gridSize.X - 1, gridSize.Y - 1)

pathsToInvestigate.Enqueue(Path(grid, startPosition))

let mutable bestPath: Path option = None

while pathsToInvestigate.Count > 0 do
    let path = pathsToInvestigate.Dequeue()

    if path.Position = endPosition && (bestPath.IsNone || bestPath.Value.HeatLoss > path.HeatLoss) then
        bestPath <- Some(path)

    let possiblePositions =
        seq {
            yield path.Left
            yield path.Right
            if path.CanMoveForward then yield path.Forward
        } 
        |> Seq.filter PositionIsWithinGrid
        |> Seq.sortBy (fun pos -> endPosition.X - pos.X + endPosition.Y - pos.Y)

    possiblePositions
        |> Seq.iter (fun nextPos ->
            let possiblePath = path.Append(nextPos)

            //if nextPos = Vector2(12,9) then
            //Console.SetCursorPosition(0,0)
            //PrintPath(grid, possiblePath)
                //printfn "Gotcha"

            let targetCell = grid[possiblePath.Position]
            let possibleHeatLoss = possiblePath.HeatLoss

            // Record the minimum heat loss into the next tile
            if targetCell.MinimumHeatLoss >= possibleHeatLoss then
                targetCell.MinimumHeatLoss <- possibleHeatLoss
                pathsToInvestigate.Enqueue(possiblePath)
            else if targetCell.MinimumHeatLoss + 5 < possibleHeatLoss then
                () // there is already a much better path to that next tile
            else
                pathsToInvestigate.Enqueue(possiblePath)

            //if bestPath.IsNone || bestPath.Value.HeatLoss > possiblePath.HeatLoss then
        )

PrintPath(grid, bestPath.Value)

printfn "[Part 1]: Best Path %s" (bestPath.Value.ToString())

printfn "[Part 1]: Minimum heat loss reaching the tile %A is %d" endPosition grid[endPosition].MinimumHeatLoss
