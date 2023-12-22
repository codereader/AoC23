open System
open Utils
open System.Collections.Generic
open System.Diagnostics

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

type DirectionInfo = {
    mutable HeatLossByRemainingStraightMoves: List<int>
}

type Cell = {
    mutable IncomingDirections: Map<Vector2, DirectionInfo>
    mutable MinimumHeatLoss: int;
    CostToEnter: int
}

let gridSize = Vector2(lines[0].Length, lines.Length)

let CreateDefaultDirectionInfo() = 
    { HeatLossByRemainingStraightMoves = new List<int>(seq { Int32.MaxValue; Int32.MaxValue; Int32.MaxValue; }) }

let CreateIncomingDirectionInfo() =
    Map.empty.
        Add(Vector2.Up, CreateDefaultDirectionInfo()).
        Add(Vector2.Down, CreateDefaultDirectionInfo()).
        Add(Vector2.Left, CreateDefaultDirectionInfo()).
        Add(Vector2.Right, CreateDefaultDirectionInfo())

let GenerateGrid() =
    seq { 0..lines.Length-1 } 
    |> Seq.collect (fun y -> seq {
            for x in 0..lines[y].Length-1 do
                yield (Vector2(x,y), {
                    IncomingDirections = CreateIncomingDirectionInfo();
                    MinimumHeatLoss = Int32.MaxValue;
                    CostToEnter = Int32.Parse((lines[y][x]).ToString())
                })
        })
    |> dict

let PositionIsWithinGrid(position: Vector2) = position.X >= 0 && position.X < gridSize.X && position.Y >= 0 && position.Y < gridSize.Y

let grid = GenerateGrid()

type Path(grid: IDictionary<Vector2, Cell>, steps: List<Vector2>, heatLoss: int) =
    let _heatLoss = heatLoss

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

    member this.Contains(pos: Vector2) = 
        this.Steps.Contains(pos)

    member this.StraightMoves =
        let mutable straightMoves = 1
        let mutable currentPos = this.Steps.Count - straightMoves - 1
        let lastDirection = this.LastDirection
        while currentPos - 1 >= 0 && 
              this.Steps[currentPos] - this.Steps[currentPos - 1] = lastDirection do
            straightMoves <- straightMoves + 1
            currentPos <- currentPos - 1
        straightMoves

    member this.CanMoveForward =
        this.StraightMoves < 3
#if false
        let pathLength = this.Steps.Count
        if pathLength < 4 then
            true
        else
            let distance = this.Steps[pathLength - 4] - this.Steps[pathLength - 1]
            if Math.Abs(distance.X) = 3 || Math.Abs(distance.Y) = 3 then false else true
#endif

    member this.HeatLoss = _heatLoss

    new(grid, startPosition:Vector2) =
        let steps = new List<Vector2>()
        steps.Add(startPosition)
        Path(grid, steps, 0)

    member this.Append(position: Vector2) =
        let steps = new List<Vector2>(this.Steps)
        steps.Add(position)
        Path(grid, steps, this.HeatLoss + grid[position].CostToEnter)

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

Console.SetCursorPosition(0,0)
PrintPath(grid, pathsToInvestigate.Peek())

let stopwatch = new Stopwatch()

stopwatch.Start()

let mutable bestPath: Path option = None
let mutable counter = 0

while pathsToInvestigate.Count > 0 do
    let path = pathsToInvestigate.Dequeue()

    if bestPath.IsNone || bestPath.Value.HeatLoss > path.HeatLoss then
        if path.Position = endPosition && (bestPath.IsNone || bestPath.Value.HeatLoss > path.HeatLoss) then
            bestPath <- Some(path)
            System.Diagnostics.Debug.WriteLine("Best loss so far = {0}, Remaining paths = {1}", bestPath.Value.HeatLoss, pathsToInvestigate.Count)

        let possiblePositions =
            seq {
                yield path.Left
                yield path.Right

                if path.CanMoveForward then
                    yield path.Forward
            } 
            |> Seq.filter (fun p -> PositionIsWithinGrid(p) && path.Contains(p) = false)
            |> Seq.sortBy (fun p -> endPosition.X - p.X + endPosition.Y - p.Y)

        possiblePositions
            |> Seq.iter (fun position ->
                let possibleHeatLoss = path.HeatLoss + grid[position].CostToEnter

                // Immediately discard path candidates if the current best path is already better
                if bestPath.IsNone || bestPath.Value.HeatLoss > possibleHeatLoss then
                    let targetCell = grid[position]

                    // Record the minimum heat loss into the next tile
                    if targetCell.MinimumHeatLoss > possibleHeatLoss then
                        targetCell.MinimumHeatLoss <- possibleHeatLoss
                        counter <- counter + 1

                        if counter % 100 = 0 then
                            Console.SetCursorPosition(path.Position.X, path.Position.Y)
                            Console.ForegroundColor <- ConsoleColor.White
                            printf "%d" targetCell.CostToEnter

                    // Whether we follow from this path depends on the information on the cell
                    // investigate whether we had this incoming direction before and
                    // whether this path is worse than that
                    let possiblePath = path.Append(position)
                    let incomingDirection = targetCell.IncomingDirections[possiblePath.LastDirection]
                    let remainingStepsForThisPath = 3 - possiblePath.StraightMoves
                    if incomingDirection.HeatLossByRemainingStraightMoves[remainingStepsForThisPath] > possibleHeatLoss then
                        incomingDirection.HeatLossByRemainingStraightMoves[remainingStepsForThisPath] <- possibleHeatLoss
                        pathsToInvestigate.Enqueue(possiblePath)
            )

stopwatch.Stop()

PrintPath(grid, bestPath.Value)

printfn "[Part 1]: Best Path %s" (bestPath.Value.ToString())

// Correct answer is 907
printfn "[Part 1]: Minimum heat loss reaching the tile %A is %d" endPosition grid[endPosition].MinimumHeatLoss
printfn "[Part 1]: This took %f seconds" (stopwatch.Elapsed.TotalSeconds)
