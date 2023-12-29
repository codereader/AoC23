open System
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

// Test input
lines <- @"...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........".Replace("\r\n", "\n").Split('\n')


let grid =
    lines
    |> Seq.map (fun line -> line.ToCharArray())
    |> Seq.toArray


let startY = grid |> Seq.findIndex (fun line -> line |> Seq.exists(fun c -> c = 'S'))
let startX = grid[startY] |> Seq.findIndex (fun ch -> ch = 'S')
let startPos = Vector2(startX, startY)

printfn "Start Position: %A" startPos

let PrintGrid(grid: char array array, reachablePositions: HashSet<Vector2>) = 
    let oldColour = Console.ForegroundColor
    let gridSize = Vector2(grid[0].Length, grid.Length)

    let maxX = reachablePositions |> Seq.map (fun p -> p.X) |> Seq.max
    let maxY = reachablePositions |> Seq.map (fun p -> p.Y) |> Seq.max
    let minX = reachablePositions |> Seq.map (fun p -> p.X) |> Seq.min
    let minY = reachablePositions |> Seq.map (fun p -> p.Y) |> Seq.min

    for y in { minY..maxY } do
        for x in { minX..maxX } do
            if reachablePositions.Contains(Vector2(x,y)) then
                Console.ForegroundColor <- ConsoleColor.White
                printf "%c" 'O'
            else
                Console.ForegroundColor <- ConsoleColor.DarkGray
                let my = y % gridSize.Y
                let mx = x % gridSize.X
                printf "%c" (grid[if my < 0 then my + gridSize.Y else my][if mx < 0 then mx + gridSize.X else mx])
        printfn ""

    Console.ForegroundColor <- oldColour

let mutable reachablePositions = new HashSet<Vector2>()
ignore (reachablePositions.Add(startPos))

let NESW = [| Vector2.Up; Vector2.Right; Vector2.Down; Vector2.Left |]
let gridSize = Vector2(grid[0].Length, grid.Length)

for round in { 1..200 } do
    let newSet = new HashSet<Vector2>()

    for pos in  reachablePositions do
        NESW
            |> Seq.map (fun dir -> pos + dir)
            //|> Seq.filter (fun p -> p.X >= 0 && p.X < gridSize.X && p.Y >= 0 && p.Y < gridSize.Y)
            |> Seq.filter (fun p ->
                let my = p.Y % gridSize.Y
                let mx = p.X % gridSize.X
                grid[if my < 0 then my + gridSize.Y else my][if mx < 0 then mx + gridSize.X else mx] <> '#')
            |> Seq.iter (fun p -> ignore (newSet.Add(p)))
        ()

    reachablePositions <- newSet

PrintGrid(grid, reachablePositions)
printfn "[Part 1]: Reachable Positions = %d" reachablePositions.Count

// Test Data:
// N=200 => 26538

