open System
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

// Test input
lines <- @"#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#".Replace("\r\n", "\n").Split('\n')

let grid =
    lines
    |> Seq.map (fun line -> line.ToCharArray())
    |> Seq.toArray

let startY = 0
let startX = grid[startY] |> Seq.findIndex (fun ch -> ch <> '#')
let startPos = Vector2(startX, startY)

let endX = grid[grid.Length - 1] |> Seq.findIndex (fun ch -> ch <> '#')
let endPos = Vector2(endX, grid.Length - 1)

let gridSize = Vector2(lines[0].Length, lines.Length)

printfn "Grid Size: %A" gridSize
printfn "Start Position: %A" startPos

type Path(steps: List<Vector2>, positions: HashSet<Vector2>) =
    member val Steps = steps with get
    member val Positions = positions with get
    member val Name = Path.GetNextName() with get
    member this.CurrentPosition = if this.Steps.Count > 0 then this.Steps[this.Steps.Count - 1] else startPos

    member val Connections = new List<Path>() with get
    member val IsRoadToExit = false with get, set

    new() =
        Path(new List<Vector2>(), new HashSet<Vector2>())

    member this.Clone() =
        let copiedHashSet = new HashSet<Vector2>()
        for p in this.Positions do ignore(copiedHashSet.Add(p))

        Path(new List<Vector2>(this.Steps), copiedHashSet)

    member this.AddStep(pos) = 
        let clone = this.Clone()

        ignore(clone.Positions.Add(pos))
        clone.Steps.Add(pos)
        clone

    member this.AddStepInPlace(pos) = 
        ignore(this.Positions.Add(pos))
        this.Steps.Add(pos)

    member this.RemoveFirstPos() =
        ignore(this.Positions.Remove(this.Steps[0]))
        this.Steps.RemoveAt(0)

    static member val NextName = 'A' with get, set

    static member private GetNextName() =
        let name = new String(Path.NextName, 1)
        Path.NextName <- char (int Path.NextName + 1)
        name

let PrintPath(path: Path, ch: char) = 
    let oldColour = Console.ForegroundColor
    Console.ForegroundColor <- ConsoleColor.White

    for pos in path.Positions do
        Console.SetCursorPosition(pos.X, pos.Y)
        printf "%s" path.Name

    Console.ForegroundColor <- oldColour

let PrintGrid(grid: char array array, path: Path) = 
    let oldColour = Console.ForegroundColor
    let gridSize = Vector2(grid[0].Length, grid.Length)

    for y in { 0..gridSize.Y - 1 } do
        for x in { 0 .. gridSize.X - 1 } do
            if path.Positions.Contains(Vector2(x,y)) then
                Console.ForegroundColor <- ConsoleColor.White
                printf "%c" 'O'
            else
                Console.ForegroundColor <- ConsoleColor.DarkGray
                printf "%c" (grid[y][x])
        printfn ""

    Console.ForegroundColor <- oldColour

let CreateNewPath(grid: char array array, pos: Vector2, path: Path) =
    match grid[pos.Y][pos.X] with
    | '.' -> Some(path.AddStep(pos)) // single step
    | '<' -> if path.Positions.Contains(pos + Vector2.Left) = false then Some(path.AddStep(pos).AddStep(pos + Vector2.Left)) else None
    | '>' -> if path.Positions.Contains(pos + Vector2.Right) = false then Some(path.AddStep(pos).AddStep(pos + Vector2.Right)) else None
    | '^' -> if path.Positions.Contains(pos + Vector2.Up) = false then Some(path.AddStep(pos).AddStep(pos + Vector2.Up)) else None
    | 'v' -> if path.Positions.Contains(pos + Vector2.Down) = false then Some(path.AddStep(pos).AddStep(pos + Vector2.Down)) else None
    | _ -> None

let NESW = [| Vector2.Up; Vector2.Right; Vector2.Down; Vector2.Left |]

let FindLongestPath(grid: char array array, startPos) =
    let pathsToInvestigate = new Queue<Path>()

    let rootPath = Path()
    ignore(rootPath.Positions.Add(startPos))

    pathsToInvestigate.Enqueue(rootPath)
    let mutable longestPath: Path option = None

    while pathsToInvestigate.Count > 0 do
        let path = pathsToInvestigate.Dequeue()

        NESW
            |> Seq.map (fun dir -> path.CurrentPosition + dir)
            |> Seq.filter (fun pos -> pos.X >= 0 && pos.X < gridSize.X && pos.Y >= 0 && pos.Y < gridSize.Y)
            |> Seq.filter (fun pos -> grid[pos.Y][pos.X] <> '#')
            |> Seq.filter (fun pos -> path.Positions.Contains(pos) = false)
            |> Seq.map (fun pos -> CreateNewPath(grid, pos, path))
            |> Seq.filter (fun newPath -> newPath.IsSome)
            |> Seq.map (fun newPath -> newPath.Value)
            |> Seq.iter (fun newPath ->
                // Found a new best path?
                if newPath.CurrentPosition = endPos && (longestPath.IsNone || longestPath.Value.Steps.Count < newPath.Steps.Count) then
                    longestPath <- Some(newPath)
                    printfn "New best path length: %d" newPath.Steps.Count
                else
                    pathsToInvestigate.Enqueue(newPath)
            )

    longestPath

#if false
let longestPathPart1 = FindLongestPath(grid, startPos)
//System.Console.SetCursorPosition(0, 2)
//PrintGrid(grid, longestPathPart1.Value)
printfn "[Part 1]: Longest path = %d" longestPathPart1.Value.Steps.Count
#endif

// Create a new grid swapping out the slopes

let modifiedGrid =
    grid
    |> Seq.map (fun line -> new String(line))
    |> Seq.map (fun line -> line.Replace('<', '.').Replace('>', '.').Replace('^', '.').Replace('v', '.'))
    |> Seq.map (fun line -> line.ToCharArray())
    |> Seq.toArray

// Preprocess the graph, finding the crossings

let GetPossibleDirections(path: Path) =
    NESW
    |> Seq.map (fun dir -> path.CurrentPosition + dir)
    |> Seq.filter (fun pos -> pos.X >= 0 && pos.X < gridSize.X && pos.Y >= 0 && pos.Y < gridSize.Y)
    |> Seq.filter (fun pos -> grid[pos.Y][pos.X] <> '#')
    |> Seq.filter (fun pos -> path.Positions.Contains(pos) = false)
    |> Seq.toArray

let GetOutgoingDirections(pos: Vector2) =
    NESW
    |> Seq.map (fun dir -> pos + dir)
    |> Seq.filter (fun pos -> pos.X >= 0 && pos.X < gridSize.X && pos.Y >= 0 && pos.Y < gridSize.Y)
    |> Seq.filter (fun pos -> grid[pos.Y][pos.X] <> '#')
    |> Seq.toArray

let allGridPositions = seq {
    for y in seq { 0.. modifiedGrid.Length-1 } do
        for x in seq { 0.. modifiedGrid[0].Length-1 } do
            yield Vector2(x, y)
    }

PrintGrid(modifiedGrid, Path())

// Find all path crossings on the map, including the start and and positions
// Since the start is on the top left and the exit is on the bottom right they end up at the head and the tail of the array
let allCrossings =
    allGridPositions
    |> Seq.filter (fun pos -> grid[pos.Y][pos.X] = '.' && (pos = startPos || pos = endPos || GetOutgoingDirections(pos).Length > 2))
    |> Seq.toArray

printfn "Found %d crossings: %A" allCrossings.Length allCrossings

let TryFindDistanceBetweenCrossings(grid: char array array, first: int, second: int) =
    let startPos = allCrossings[first]
    let endPos = allCrossings[second]
    let otherCrossings =
        seq { 0 .. allCrossings.Length - 1 }
        |> Seq.filter (fun i -> i <> first && i <> second)
        |> Seq.map (fun i -> allCrossings[i])
        |> Set.ofSeq

    let positionsToInvestigate = new Queue<Tuple<Vector2, int>>()
    positionsToInvestigate.Enqueue((startPos, 0))

    let mutable foundDistance: int option = None
    let investigatedPositions = new HashSet<Vector2>()
    ignore(investigatedPositions.Add(startPos))

    while positionsToInvestigate.Count > 0 && foundDistance.IsNone do

        let (pos, dist) = positionsToInvestigate.Dequeue()

        NESW
            |> Seq.map (fun dir -> pos + dir)
            |> Seq.filter (fun pos -> pos.X >= 0 && pos.X < gridSize.X && pos.Y >= 0 && pos.Y < gridSize.Y)
            |> Seq.filter (fun pos -> grid[pos.Y][pos.X] <> '#')
            |> Seq.filter (fun pos -> investigatedPositions.Contains(pos) = false)
            |> Seq.filter (fun pos -> otherCrossings.Contains(pos) = false)
            |> Seq.iter (fun pos -> 
                if pos = endPos then
                    foundDistance <- Some(dist + 1)
                else
                    positionsToInvestigate.Enqueue((pos, dist + 1))
                    ignore(investigatedPositions.Add(pos))
            )
        ()

    foundDistance

type Connection = { Begin: int; End: int; Length: int }

let connections =
    seq {
    // Connect all crossings to their first neighbour, connect by index
    for first in { 0 .. allCrossings.Length - 1 } do
        for second in { 0 .. allCrossings.Length - 1 } do
            if first <> second then
                let distanceBetweenCrossings = TryFindDistanceBetweenCrossings(modifiedGrid, first, second)
                if distanceBetweenCrossings.IsSome then
                    yield { Begin = first; End = second; Length = distanceBetweenCrossings.Value } // Create a new connection between these two crossings
    }
    |> Seq.toArray

printfn "Found %d connections" connections.Length
for conn in connections do
    printfn "Connection: %A" conn

#if false
let ExtendPath(path:Path) =
    let mutable possibleDirections = GetPossibleDirections(path)

    while possibleDirections.Length = 1 do
        path.AddStepInPlace(possibleDirections[0])
        possibleDirections <- GetPossibleDirections(path)

    if path.CurrentPosition = endPos then
        path.IsRoadToExit <- true

Path.NextName <- 'A'
let pathsToExtend = new Queue<Path>()
let startPath = Path().AddStep(startPos)
pathsToExtend.Enqueue(startPath)

let crossings = new Dictionary<Vector2, HashSet<Path>>()

while pathsToExtend.Count > 0 do
    let path = pathsToExtend.Dequeue()
    ExtendPath(path)

    //System.Console.SetCursorPosition(0, 0)
    //PrintGrid(modifiedGrid, path)

    let startPos = path.Steps[0]
    let endPos = path.CurrentPosition

    if crossings.ContainsKey(startPos) = false then
        crossings[startPos] <- new HashSet<Path>()

    if crossings.ContainsKey(endPos) = false then
        crossings[endPos] <- new HashSet<Path>()

        // Spawn off new paths to extend from this new crossing
        let possibleDirections = GetPossibleDirections(path)

        possibleDirections
            |> Seq.map (fun pos -> Path().AddStep(endPos).AddStep(pos))
            |> Seq.iter (fun followUpPath ->
                pathsToExtend.Enqueue(followUpPath))

    // Register this extended path in the start and end positions
    if crossings[startPos] |> Seq.exists (fun p -> p.Positions.SetEquals(path.Positions)) = false then
        ignore(crossings[startPos].Add(path))

    if crossings[endPos] |> Seq.exists (fun p -> p.Positions.SetEquals(path.Positions)) = false then
        ignore(crossings[endPos].Add(path))

        //for toExtend in pathsToExtend do
        //    System.Console.ForegroundColor <- System.ConsoleColor.DarkGreen
        //    System.Console.SetCursorPosition(toExtend.CurrentPosition.X, toExtend.CurrentPosition.Y)
        //    System.Console.Write('X')
    ()

printfn "Start Path Connections: %d" startPath.Connections.Count

#if false
for kvp in crossings do
    System.Console.SetCursorPosition(0,0)
    PrintGrid(modifiedGrid, Path())
    System.Console.SetCursorPosition(kvp.Key.X, kvp.Key.Y)
    printf "%c" 'X'

    let mutable ch = '1'
    for path in kvp.Value do
        PrintPath(path, ch)
        ch <- char (int ch + 1)
    ()
#endif

type PathSequence() =
    member val Paths = new HashSet<Path>() with get
    member val Steps = new List<Path>() with get
    member val Length = 0 with get, set
    member this.LastPath = this.Steps[this.Steps.Count - 1]
    member val LastPosition = Vector2(-1,-1) with get, set

    member this.AddPath(path: Path) =
        
        if this.LastPosition = Vector2(-1,-1)  then
            this.LastPosition <- path.CurrentPosition
        else if this.LastPosition = path.Steps[0] then
            // The path is continuing right from the last sequence
            this.LastPosition <- path.CurrentPosition
        else
            // The path seems to be connected back-to-front
            this.LastPosition <- path.Steps[0]
            
        ignore(this.Paths.Add(path))
        this.Steps.Add(path)
        this.Length <- this.Length + path.Steps.Count - 1 // Paths are always including the crossing they started from

    member this.WithPath(path) =
        let clone = PathSequence()
        for p in this.Steps do
            ignore(clone.AddPath(p))
        clone.AddPath(path)
        clone

let PrintPathSequence(grid: char array array, sequence: PathSequence) =
    System.Console.SetCursorPosition(0,0)
    PrintGrid(modifiedGrid, sequence.Steps[0])

    let mutable ch = '1'
    for path in sequence.Steps do
        PrintPath(path, ch)
        ch <- char (int ch + 1)
    System.Console.SetCursorPosition(0, gridSize.Y + 1)

let FindLongestPathPart2(startPath) =

    let seqsToInvestigate = new Stack<PathSequence>()

    let rootPath = PathSequence()
    rootPath.AddPath(startPath)

    seqsToInvestigate.Push(rootPath)
    let mutable longestSeq: PathSequence option = None

    while seqsToInvestigate.Count > 0 do
        let sequence = seqsToInvestigate.Pop()

        //PrintPathSequence(modifiedGrid, sequence)
        //printfn "Current Length: %d" sequence.Length

        let foundCrossing, outgoingPaths = crossings.TryGetValue(sequence.LastPosition)

        if foundCrossing then
            let roadToExit = outgoingPaths |> Seq.tryFind _.IsRoadToExit

            // We have to take the road to the exit, every other road is going to block off the path to the exit
            let pathsToInvestigate = if roadToExit.IsSome then seq { roadToExit.Value } else outgoingPaths

            pathsToInvestigate
            |> Seq.filter (fun path -> sequence.Paths.Contains(path) = false)
            |> Seq.map (fun path -> sequence.WithPath(path))
            |> Seq.iter (fun newSequence ->
                // Found a new best path?
                if newSequence.LastPath.IsRoadToExit && (longestSeq.IsNone || longestSeq.Value.Length < newSequence.Length) then
                    longestSeq <- Some(newSequence)

                    System.Console.Clear()
                    PrintPathSequence(modifiedGrid, newSequence)

                    System.Console.SetCursorPosition(0, gridSize.Y + 1)
                    printfn "Found new best path sequence with length: %d" newSequence.Length
                else
                    seqsToInvestigate.Push(newSequence)
            )

    longestSeq

let stopwatch = new Stopwatch()
stopwatch.Start()

let bestSequence = FindLongestPathPart2(startPath)

stopwatch.Stop()

PrintPathSequence(modifiedGrid, bestSequence.Value)

System.Console.SetCursorPosition(0, gridSize.Y + 1)
printfn "[Part 2]: Longest path = %d" bestSequence.Value.Length
printfn "[Part 2]: This took %f seconds" stopwatch.Elapsed.TotalSeconds
#endif
