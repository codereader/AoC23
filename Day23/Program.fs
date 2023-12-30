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
    member this.CurrentPosition = if this.Steps.Count > 0 then this.Steps[this.Steps.Count - 1] else startPos

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

let pathsToInvestigate = new Stack<Path>()

let rootPath = Path()
ignore(rootPath.Positions.Add(startPos))

pathsToInvestigate.Push(rootPath)
let mutable longestPath: Path option = None

let NESW = [| Vector2.Up; Vector2.Right; Vector2.Down; Vector2.Left |]

let CreateNewPath(pos: Vector2, path: Path) =
    match grid[pos.Y][pos.X] with
    | '.' -> Some(path.AddStep(pos)) // single step
    | '<' -> if path.Positions.Contains(pos + Vector2.Left) = false then Some(path.AddStep(pos).AddStep(pos + Vector2.Left)) else None
    | '>' -> if path.Positions.Contains(pos + Vector2.Right) = false then Some(path.AddStep(pos).AddStep(pos + Vector2.Right)) else None
    | '^' -> if path.Positions.Contains(pos + Vector2.Up) = false then Some(path.AddStep(pos).AddStep(pos + Vector2.Up)) else None
    | 'v' -> if path.Positions.Contains(pos + Vector2.Down) = false then Some(path.AddStep(pos).AddStep(pos + Vector2.Down)) else None
    | _ -> None

while pathsToInvestigate.Count > 0 do
    let path = pathsToInvestigate.Pop()

    NESW
        |> Seq.map (fun dir -> path.CurrentPosition + dir)
        |> Seq.filter (fun pos -> pos.X >= 0 && pos.X < gridSize.X && pos.Y >= 0 && pos.Y < gridSize.Y)
        |> Seq.filter (fun pos -> grid[pos.Y][pos.X] <> '#')
        |> Seq.filter (fun pos -> path.Positions.Contains(pos) = false)
        |> Seq.map (fun pos -> CreateNewPath(pos, path))
        |> Seq.filter (fun newPath -> newPath.IsSome)
        |> Seq.map (fun newPath -> newPath.Value)
        |> Seq.iter (fun newPath ->
            // Found a new best path?
            if newPath.CurrentPosition = endPos && (longestPath.IsNone || longestPath.Value.Steps.Count < newPath.Steps.Count) then
                longestPath <- Some(newPath)
            
            pathsToInvestigate.Push(newPath)
        )

if longestPath.IsSome then
    System.Console.SetCursorPosition(0, 2)
    PrintGrid(grid, longestPath.Value)
    printfn "[Part 1]: Longest path = %d" longestPath.Value.Steps.Count
