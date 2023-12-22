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
    let Grid = grid
    let Steps = steps
    let heatLoss = heatLoss
    let position = steps[steps.Count - 1]
    let lastDirection = 
        if steps.Count > 1 then
            position - steps[steps.Count - 2]
        else
            Vector2.Right

    let rightDirection =
        Vector2(lastDirection.Y, lastDirection.X * -1)

    let leftDirection =
        Vector2(-rightDirection.X, -rightDirection.Y)

    let right = position + rightDirection
    let left = position + leftDirection
    let forward = position + lastDirection
    let straightMoves =
        let mutable straightMoves = 1
        let mutable currentPos = Steps.Count - straightMoves - 1
        let lastDirection = lastDirection
        while currentPos - 1 >= 0 && 
              Steps[currentPos] - Steps[currentPos - 1] = lastDirection do
            straightMoves <- straightMoves + 1
            currentPos <- currentPos - 1
        straightMoves

    member this.Position = position
    member this.StraightMoves = straightMoves
    
    member this.Left = left
    member this.Right = right
    member this.Forward = forward
    member this.LeftDirection = leftDirection
    member this.RightDirection = rightDirection
    member this.ForwardDirection = lastDirection

    member this.Contains(pos: Vector2) = steps.Contains(pos)

    member this.CanMoveForward = straightMoves < 3
    
    member this.HeatLoss = heatLoss

    new(grid, startPosition:Vector2) =
        let steps = new List<Vector2>()
        steps.Add(startPosition)
        Path(grid, steps, 0)

    member this.Append(position: Vector2) =
        let steps = new List<Vector2>(Steps)
        steps.Add(position)
        Path(grid, steps, this.HeatLoss + grid[position].CostToEnter)

    override this.ToString() =
        sprintf "%s" (String.Join(" > ", steps |> Seq.map(fun step -> step.ToString())))

    member this.Print() = 
        let oldColour = Console.ForegroundColor

        for y in { 0..gridSize.Y-1 } do
            for x in { 0..gridSize.X-1 } do
                let pos = Vector2(x,y)
                let cell = grid[pos]
                if steps |> Seq.exists (fun step -> step = pos) then
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
pathsToInvestigate.Peek().Print()

let stopwatch = new Stopwatch()

stopwatch.Start()

let mutable bestPath: Path option = None
let mutable counter = 0

let part = 1

if part = 1 then
    while pathsToInvestigate.Count > 0 do
        let path = pathsToInvestigate.Dequeue()

        if bestPath.IsNone || bestPath.Value.HeatLoss > path.HeatLoss then
            if path.Position = endPosition && (bestPath.IsNone || bestPath.Value.HeatLoss > path.HeatLoss) then
                bestPath <- Some(path)
                System.Diagnostics.Debug.WriteLine("Best loss so far = {0}, Remaining paths = {1}", bestPath.Value.HeatLoss, pathsToInvestigate.Count)

            let possiblePositions =
                seq {
                    yield (1, path.Left, path.LeftDirection)
                    yield (1, path.Right, path.RightDirection)

                    if path.CanMoveForward then
                        yield (path.StraightMoves + 1, path.Forward, path.ForwardDirection)
                } 
                |> Seq.filter (fun (_, p, _) -> PositionIsWithinGrid(p) && path.Contains(p) = false)
                //|> Seq.sortBy (fun (_, p, _) -> endPosition.X - p.X + endPosition.Y - p.Y)

            possiblePositions
                |> Seq.iter (fun (straightMoves, position, lastDirection) ->
                    let possibleHeatLoss = path.HeatLoss + grid[position].CostToEnter

                    // Immediately discard path candidates if the current best path is already better
                    if bestPath.IsNone || bestPath.Value.HeatLoss > possibleHeatLoss then
                        let targetCell = grid[position]

                        // Record the minimum heat loss into the next tile
                        if targetCell.MinimumHeatLoss > possibleHeatLoss then
                            targetCell.MinimumHeatLoss <- possibleHeatLoss
                            counter <- counter + 1

                            if counter % 1000 = 0 then
                                Console.SetCursorPosition(path.Position.X, path.Position.Y)
                                Console.ForegroundColor <- ConsoleColor.White
                                printf "%d" targetCell.CostToEnter

                        let PositionIsWorthInvestigating(targetCell, incomingDirection, straightMoves) =
                            let remainingStepsForThisPath = 3 - straightMoves
                            seq { remainingStepsForThisPath .. 2 }
                                |> Seq.exists (fun remainingSteps -> possibleHeatLoss >= incomingDirection.HeatLossByRemainingStraightMoves[remainingSteps]) = false

                        // Whether we follow from this path depends on the information on the cell
                        // investigate whether we had this incoming direction before and
                        // whether this path is worse than that
                        let remainingStepsForThisPath = 3 - straightMoves
                        let incomingDirection = targetCell.IncomingDirections[lastDirection]

                        if PositionIsWorthInvestigating(targetCell, incomingDirection, straightMoves) then
                            pathsToInvestigate.Enqueue(path.Append(position))

                            if incomingDirection.HeatLossByRemainingStraightMoves[remainingStepsForThisPath] > possibleHeatLoss then
                                incomingDirection.HeatLossByRemainingStraightMoves[remainingStepsForThisPath] <- possibleHeatLoss
                )
else if part = 2 then
    ()

stopwatch.Stop()

Console.SetCursorPosition(0,0)
bestPath.Value.Print()

printfn "[Part %d]: Best Path %s" part (bestPath.Value.ToString())

// Correct answer for Part 1 is 907
printfn "[Part %d]: Minimum heat loss reaching the tile %A is %d" part endPosition grid[endPosition].MinimumHeatLoss
printfn "[Part %d]: This took %f seconds" part (stopwatch.Elapsed.TotalSeconds)
