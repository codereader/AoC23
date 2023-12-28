open System
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
// Test input
lines <- @"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)".Replace("\r\n", "\n").Split('\n')
*)


let startPos = Vector2(0,0)

let GetDirection(str) =
    match str with
    | "U" -> Vector2.Up
    | "L" -> Vector2.Left
    | "R" -> Vector2.Right
    | "D" -> Vector2.Down
    | _ -> failwith "Unknown direction"

let mutable min = Vector2(0,0)
let mutable max = Vector2(0,0)

let mutable currentPos = startPos

let commands =
    lines
    |> Seq.map (fun line -> line.Split(' '))
    |> Seq.map (fun parts ->
        let direction = GetDirection(parts[0]) * (int parts[1])
        currentPos <- currentPos + direction
        min.X <- Math.Min(min.X, currentPos.X)
        min.Y <- Math.Min(min.Y, currentPos.Y)
        max.X <- Math.Max(max.X, currentPos.X)
        max.Y <- Math.Max(max.Y, currentPos.Y)
        direction
        )
    |> Seq.toArray

printfn "%A" commands

commands
    |> Seq.iter (fun vec -> currentPos <- currentPos + vec)

printfn "%A to %A, Bounds (%A %A)" startPos currentPos min max

let gridSize = Vector2(max.X - min.X + 1, max.Y - min.Y + 1)

printfn "Grid size %A" gridSize
let gridStartPos = Vector2(0 - min.X, 0 - min.Y)

currentPos <- gridStartPos
min <- Vector2(0,0)
max <- Vector2(0,0)

let grid =
    seq { 0 .. gridSize.Y - 1 }
    |> Seq.map (fun y -> Array.init gridSize.X (fun i -> '.'))
    |> Seq.toArray

printfn "Translated Grid: %A to %A, Bounds (%A %A)" gridStartPos currentPos min max

lines
|> Seq.map (fun line -> line.Split(' '))
|> Seq.iter (fun parts ->
    let direction = GetDirection(parts[0])
    for i in seq { 1 .. int parts[1] } do
        currentPos <- currentPos + direction
        grid[currentPos.Y][currentPos.X] <- '#'
    )

let PrintGrid() = 
    grid |> Seq.iter (fun chars -> printfn "%s" (new String(chars)))

let PositionIsWithinGrid(position: Vector2) = position.X >= 0 && position.X < gridSize.X && position.Y >= 0 && position.Y < gridSize.Y

let FloodFillGrid(startPos, fillChar) =
    let mutable positionsToCover = new Stack<Vector2>()
    positionsToCover.Push(startPos)

    while positionsToCover.Count > 0 do
        let position = positionsToCover.Pop()
        grid[position.Y][position.X] <- fillChar

        seq { Vector2.Up; Vector2.Down; Vector2.Right; Vector2.Left }
            |> Seq.map (fun dir -> position + dir)
            |> Seq.filter (fun pos -> PositionIsWithinGrid(pos) && grid[pos.Y][pos.X] = '.')
            |> Seq.iter (fun pos -> positionsToCover.Push(pos))

let gridEdges = seq {
    for y in seq { 0 .. gridSize.Y - 1 } do
        yield Vector2(0,y)
        yield Vector2(gridSize.X-1,y)
    for x in seq { 0 .. gridSize.X - 1 } do
        yield Vector2(x,0)
        yield Vector2(x,gridSize.Y-1)
}

gridEdges
    |> Seq.filter (fun edgePoint -> grid[edgePoint.Y][edgePoint.X] = '.')
    |> Seq.iter (fun edgePoint -> FloodFillGrid(edgePoint, 'O'))

PrintGrid()

let lagoonVolume = 
    seq { 0 .. gridSize.Y - 1 }
    |> Seq.map (fun y -> (grid[y] |> Seq.filter (fun ch -> ch = '.' || ch = '#') |> Seq.length))
    |> Seq.sum

printfn "[Part 1]: Lagoon volume= %d cubic units" lagoonVolume // 48400

// Part 2: Get the instruction from the colour

let GetDirectionPart2(str) =
    match str with
    | "3" -> Vector2.Up
    | "2" -> Vector2.Left
    | "0" -> Vector2.Right
    | "1" -> Vector2.Down
    | _ -> failwith "Unknown direction"

currentPos <- Vector2(0,0)
min <- Vector2(0,0)
max <- Vector2(0,0)

type Edge = { StartPos: Vector2; EndPos: Vector2; Edge: Vector2 }

#if false
let edges =
    lines
    |> Seq.map (fun line -> line.Split(' '))
    |> Seq.map (fun parts ->
        let edge = GetDirection(parts[0]) * (int parts[1])
        let startPoint = currentPos
        let endPoint = startPoint + edge

        currentPos <- endPoint
        min.X <- Math.Min(min.X, currentPos.X)
        min.Y <- Math.Min(min.Y, currentPos.Y)
        max.X <- Math.Max(max.X, currentPos.X)
        max.Y <- Math.Max(max.Y, currentPos.Y)
        { StartPos = startPoint; EndPos = endPoint; Edge = edge }
        )
    |> Seq.toArray
#else
let edges =
    lines
    |> Seq.map (fun line -> line.TrimEnd(')').Split('#', StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (fun parts ->
        let length = Convert.ToInt32(parts[1].Substring(0,5), 16)
        let dir = GetDirectionPart2(parts[1].Substring(5))
        let edge = dir * length

        let startPoint = currentPos
        let endPoint = startPoint + edge
        min.X <- Math.Min(min.X, endPoint.X)
        min.Y <- Math.Min(min.Y, endPoint.Y)
        max.X <- Math.Max(max.X, endPoint.X)
        max.Y <- Math.Max(max.Y, endPoint.Y)
        currentPos <- endPoint
        { StartPos = startPoint; EndPos = endPoint; Edge = edge }
        )
    |> Seq.toArray
#endif

printfn "%A edges" edges.Length

printfn "%A to %A, Bounds (%A %A)" startPos currentPos min max

let allHorizontalEdges = 
    edges
    |> Seq.filter (fun e -> e.Edge.Y = 0)
    |> Seq.toArray

printfn "%A horizontal edges" allHorizontalEdges.Length

// Full volume: accumulate all horizontal edge lengths times their distance from the x axis

let volumeA = 
    allHorizontalEdges
    |> Seq.sumBy (fun edge -> int64 edge.Edge.X * int64 edge.StartPos.Y)

let edgeLengthSum =
    edges
    |> Seq.sumBy (fun edge -> Math.Abs(int64 edge.Edge.X) + Math.Abs(int64 edge.Edge.Y))

let innerPoints = Math.Abs(volumeA) - (edgeLengthSum / 2L - 1L)

printfn "[Part 2]: A = %d" volumeA
printfn "[Part 2]: Edge Lengths = %d" edgeLengthSum
printfn "[Part 2]: Inner Points (Pick) = %d" innerPoints

let volume = Math.Abs(volumeA) + edgeLengthSum / 2L + 1L

printfn "[Part 2]: Volume = %d" volume

