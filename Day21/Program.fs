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

    let maxX = reachablePositions |> Seq.map (fun p -> p.X) |> Seq.max
    let maxY = reachablePositions |> Seq.map (fun p -> p.Y) |> Seq.max
    let minX = reachablePositions |> Seq.map (fun p -> p.X) |> Seq.min
    let minY = reachablePositions |> Seq.map (fun p -> p.Y) |> Seq.min

    printfn "Grid Dimensions: %d|%d to %d|%d" minX minY maxX maxY

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

let PositionIsStone(grid: char array array, x,y) =
    let my = y % gridSize.Y
    let mx = x % gridSize.X
    grid[if my < 0 then my + gridSize.Y else my][if mx < 0 then mx + gridSize.X else mx] = '#'

for round in { 1..64 } do
    let newSet = new HashSet<Vector2>()

    for pos in  reachablePositions do
        NESW
            |> Seq.map (fun dir -> pos + dir)
            //|> Seq.filter (fun p -> p.X >= 0 && p.X < gridSize.X && p.Y >= 0 && p.Y < gridSize.Y)
            |> Seq.filter (fun p -> PositionIsStone(grid, p.X, p.Y) = false)
            |> Seq.iter (fun p -> ignore (newSet.Add(p)))
        ()

    reachablePositions <- newSet

//PrintGrid(grid, reachablePositions)
printfn "[Part 1]: Reachable Positions = %d" reachablePositions.Count

// Part 2

// The assumption here is that by increasing the number of steps by a multiple of the map width
// the resulting grid pattern is repeating - numbers get larger, but the diamond shapes are roughly the same.
// Assume the number of reachable positions is following a quadratic equation with three coefficients a, b and c
// We can find these coefficients by calculating the result for three distinct numbers of steps n1, n2 and n3

// Calculate the needed data points, setting N = TileWidth * (k + 1/2) with a few example values for k
let n = new List<int>()
let r = new List<int>()

let mutable positions = new HashSet<Vector2>()
ignore (positions.Add(startPos))

let maxK = 4
let steps = gridSize.X * maxK + gridSize.X / 2
printfn "[Part 2]: Hang tight, calculating reachabilities for %d steps..." steps

for round in { 1..steps } do
    let newSet = new HashSet<Vector2>()

    for pos in  positions do
        NESW
            |> Seq.map (fun dir -> pos + dir)
            //|> Seq.filter (fun p -> p.X >= 0 && p.X < gridSize.X && p.Y >= 0 && p.Y < gridSize.Y)
            |> Seq.filter (fun p -> PositionIsStone(grid, p.X, p.Y) = false)
            |> Seq.iter (fun p -> ignore (newSet.Add(p)))
        ()

    positions <- newSet

    // On an even number of grid size multiples, yield the results
    if (round - gridSize.X / 2) % (gridSize.X * 2) = 0 then
        let k = (round - gridSize.X / 2) / gridSize.X
        printfn "[Part 2]: Reachable Positions for k = %d (steps = %d): result = %d" k round positions.Count
        n.Add(round)
        r.Add(positions.Count)

// Results
// [Part 2]: Reachable Positions for k = 2 (steps = 327): result = 93148
// [Part 2]: Reachable Positions for k = 4 (steps = 589): result = 301602
// [Part 2]: Reachable Positions for k = 6 (steps = 851): result = 629104

let r1 = float r[0] // 93148.0
let r2 = float r[1] // 301602.0
let r3 = float r[2] // 629104.0

let n1 = float n[0] // 327.0
let n2 = float n[1] // 589.0
let n3 = float n[2] // 851.0

let b = ((r1-r2)/(n1*n1-n2*n2) - (r1-r3)/(n1*n1-n3*n3)) / ((n1-n2)/(n1*n1-n2*n2) - (n1-n3)/(n1*n1-n3*n3))
let a = (r1-r2)/(n1*n1-n2*n2) - b * ((n1-n2)/(n1*n1 - n2*n2))
let c = r1 - b*n1 - a*n1*n1

printfn "[Part 2]: Coefficient a = %f" a
printfn "[Part 2]: Coefficient b = %f" b
printfn "[Part 2]: Coefficient c = %f" c

// Steps required: 26501365 (= 202300 * 131 + 65)
let neededSteps = 26501365.0
let result = a * neededSteps * neededSteps + b * neededSteps + c

printfn "[Part 2]: Reachabilities for %d steps = %d" (int64 neededSteps) (int64 result) // 609012263058042
