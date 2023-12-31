open System
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
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
*)

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

type PathSequence = { Crossings: int Set; LastCrossing: int; Length: int }

let FindLongestPathPart2() =
    let startPath = { Crossings = seq { 0 } |> Set.ofSeq; LastCrossing = 0; Length = 0 }

    let pathsToInvestigate = new Stack<PathSequence>()
    pathsToInvestigate.Push(startPath)

    let mutable longestPath: PathSequence option = None

    while pathsToInvestigate.Count > 0 do
        let path = pathsToInvestigate.Pop()

        if path.LastCrossing = allCrossings.Length - 1 && (longestPath.IsNone || longestPath.Value.Length < path.Length) then
            longestPath <- Some(path)
            printfn "Found a new longest path = %d" longestPath.Value.Length
        
        connections
            |> Seq.filter (fun conn -> conn.Begin = path.LastCrossing) // try to go from here
            |> Seq.filter (fun conn -> path.Crossings.Contains(conn.End) = false) // don't go anywhere we've already been
            |> Seq.map (fun conn -> { Crossings = path.Crossings.Add(conn.End); LastCrossing = conn.End; Length = path.Length + conn.Length })
            |> Seq.iter (fun path -> pathsToInvestigate.Push(path))
        ()

    longestPath.Value

let stopwatch = new Stopwatch()
stopwatch.Start()
let longestPathPart2 = FindLongestPathPart2()
stopwatch.Stop()

printfn "[Part 2]: Longest path = %d" longestPathPart2.Length
printfn "[Part 2]: This took %f seconds" stopwatch.Elapsed.TotalSeconds
