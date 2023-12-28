open System
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
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
*)

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

    for y in { 0..gridSize.Y-1 } do
        for x in { 0..gridSize.X-1 } do
            if reachablePositions.Contains(Vector2(x,y)) then
                Console.ForegroundColor <- ConsoleColor.White
                printf "%c" 'O'
            else
                Console.ForegroundColor <- ConsoleColor.DarkGray
                printf "%c" (grid[y][x])
        printfn ""

    Console.ForegroundColor <- oldColour

let mutable reachablePositions = new HashSet<Vector2>()
reachablePositions.Add(startPos)

PrintGrid(grid, reachablePositions)

let NESW = [| Vector2.Up; Vector2.Right; Vector2.Down; Vector2.Left |]
let gridSize = Vector2(grid[0].Length, grid.Length)

for round in { 1..64 } do
    let newSet = new HashSet<Vector2>()

    for pos in  reachablePositions do
        NESW
            |> Seq.map (fun dir -> pos + dir)
            |> Seq.filter (fun p -> p.X >= 0 && p.X < gridSize.X && p.Y >= 0 && p.Y < gridSize.Y)
            |> Seq.filter (fun p -> grid[p.Y][p.X] <> '#')
            |> Seq.iter (fun p -> ignore (newSet.Add(p)))
        ()

    reachablePositions <- newSet

PrintGrid(grid, reachablePositions)
printfn "[Part 1]: Reachable Positions = %d" reachablePositions.Count