open System
open Utils
open Day10
open System.Collections.Generic

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"
(*
lines <- "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........".Replace("\r\n", "\n").Split('\n')
*)

let pipeMap =
    seq {
        for y in { 0..lines.Length - 1 } do
            let line = lines[y]
            for x in { 0..line.Length - 1 } do
                yield (Vector2(x,y), line[x]) }
    |> Seq.map (fun (pos, ch) -> pos, ch)
    |> dict

let startPos = 
    pipeMap
    |> Seq.filter (fun pair -> pair.Value = 'S')
    |> Seq.map (fun pair -> pair.Key)
    |> Seq.head

let North = Vector2(0,-1)
let South = Vector2(0,1)
let West = Vector2(-1,0)
let East = Vector2(1,0)

let NESW =
    seq { North; East; South; West }
    |> Seq.toArray

let ValidPipesForIncomingDirection =
    dict[
        West, [| 'F'; 'L'; '-' |];
        East, [| 'J'; '7'; '-' |];
        South, [| 'L'; 'J'; '|' |];
        North, [| 'F'; '7'; '|' |];
    ]

let ValidOutgoingDirectionsForPipe =
    dict[
       '-',  [| West; East |];
       '|',  [| North; South |];
       'F',  [| South; East |];
       'J',  [| North; West |];
       'L',  [| North; East |];
       '7',  [| South; West |];
       'S',  [| North; West; South; East |];
    ]

let mutable network = Set.empty<Vector2>
let startNode = NetworkNode(startPos)

network <- network.Add(startPos)

let mutable networkNodes = List.empty<NetworkNode>

let OutgoingDirectionIsValid(pipeChar: char, direction) =
    ValidOutgoingDirectionsForPipe[pipeChar] |> Array.contains direction

let TargetPipeMatchesDirection(pipeChar: char, direction) =
    ValidPipesForIncomingDirection[direction] |> Array.contains pipeChar

let TargetDirectionIsValid(position, direction) =
    let newPos = position + direction
    network.Contains(newPos) = false &&
        pipeMap.ContainsKey(newPos) &&
        OutgoingDirectionIsValid(pipeMap[position], direction) &&
        TargetPipeMatchesDirection(pipeMap[newPos], direction)

let TryFindValidDirection(node:NetworkNode) =
    NESW |> Seq.tryFind (fun dir -> TargetDirectionIsValid(node.Position, dir))

let ExpandNetworkFromPosition(node:NetworkNode) =
    
    let mutable currentNode = node

    // Find the first valid direction that has not been investigated yet
    let mutable validDirection = TryFindValidDirection(currentNode)

    while validDirection.IsSome do
        let newPos = currentNode.Position + validDirection.Value
        //printfn "Expand from %A to %A" currentNode.Position newPos

        network <- network.Add(newPos)

        let newNode = NetworkNode(newPos)
        networkNodes <- networkNodes @ [newNode]
        currentNode.Forward <- Some(newNode)
        newNode.Back <- Some(currentNode)

        currentNode <- newNode
        validDirection <- TryFindValidDirection(currentNode)

networkNodes <- networkNodes @ [startNode]
ExpandNetworkFromPosition(startNode)

// Link the last found node in the forward sequence to the start node
let rec FindForwardEnd(node:NetworkNode) =
    let next = node.Forward
    if next.IsSome then FindForwardEnd(next.Value) else node

let lastNode = FindForwardEnd startNode
startNode.Back <- Some(lastNode)
lastNode.Forward <- Some(startNode)

printfn "Start Position: %A" startPos

// Print the map with the pipe network coloured in red
let defaultColour = Console.ForegroundColor

for y in { 0..lines.Length - 1 } do
    let line = lines[y]
    for x in { 0..line.Length - 1 } do
        if network.Contains(Vector2(x,y)) then
            Console.ForegroundColor <- ConsoleColor.Red
        else
            Console.ForegroundColor <- defaultColour
        printf "%c" pipeMap[Vector2(x,y)]
    printfn ""

// Check all the distances of all network nodes by moving forward and backward from the start
let rec CollectDistances(node:NetworkNode, forward: bool, distance: int) =
    let next = if forward then node.Forward else node.Back
    node.DistanceToStart <- System.Math.Min(node.DistanceToStart, distance)
    if next.IsSome && next.Value <> startNode then
        CollectDistances(next.Value, forward, distance + 1)

do CollectDistances(startNode, true, 0)
do CollectDistances(startNode, false, 0)

let farthestNode = networkNodes |> Seq.maxBy (fun node -> node.DistanceToStart)

printfn "[Part 1] Farthest Node at %A with distance %d" farthestNode.Position farthestNode.DistanceToStart

// Expand the grid to make room for the spaces in between the pipes

let maxY = lines.Length - 1
let maxX = lines[0].Length - 1

let mutable spacedMap = new Dictionary<Vector2f, char>()

let spacedY = seq {
    for y in seq { 0..maxY } do 
        yield (float y)
        yield (float y) + 0.5 }

let spacedX = seq {
    for x in seq { 0..maxX } do 
        yield (float x)
        yield (float x) + 0.5 }

let spacedLayout = seq {
    for y in spacedY do
        for x in spacedX do
            yield Vector2f(x, y) }

spacedLayout |> Seq.iter (fun pos -> spacedMap.Add(pos, '.'))

let PrintSpacedMap() =
    for y in spacedY do
        for x in spacedX do
            printf "%c" spacedMap[Vector2f(x,y)]
        printfn ""

let FloatVector(input: Vector2) =
    Vector2f(float input.X, float input.Y)

let GetConnectionChar(node, next) =
    'X'

let FillPipe() =
    let mutable node = Some(startNode)
    let startFloatPos = FloatVector node.Value.Position
    spacedMap[startFloatPos] <- 'S'

    while node.IsSome do
        let floatPos = FloatVector node.Value.Position
        let pipeChar = pipeMap[node.Value.Position]
        spacedMap[floatPos] <- pipeChar

        // Check the east
        let eastPos = node.Value.Position + East

        let next = node.Value.Forward
        if next.IsSome then
            let connectionChar = GetConnectionChar(node.Value, next.Value)
            let nextDirection = FloatVector(next.Value.Position - node.Value.Position)
            spacedMap[floatPos + nextDirection / 2.0] <- connectionChar
            node <- if next.Value <> startNode then next else None
        else
            node <- None

FillPipe()

// Find a position next to the border of the map
let BorderPositions = seq {
    for y in spacedY do
        yield Vector2f(0,y)
        yield Vector2f(float maxX,y)
    for x in spacedX do
        yield Vector2f(x,0)
        yield Vector2f(x, float maxY)
    }

let borderTile = 
    BorderPositions
    |> Seq.find (fun pos -> spacedMap[pos] = '.')

spacedMap[borderTile] <- 'O'

let NESWf =
    NESW
    |> Seq.map FloatVector
    |> Seq.map (fun vec -> vec / 2.0)
    |> Seq.toArray

let mutable investigatedPositions = Set.empty<Vector2f>
investigatedPositions <- investigatedPositions.Add(borderTile)

let mutable positionsToCheck = NESWf |> Seq.map (fun dir -> borderTile + dir) |> Seq.toList

PrintSpacedMap()

while positionsToCheck.Length > 0 do
    let pos = positionsToCheck.Head

    positionsToCheck <- positionsToCheck.Tail

    if investigatedPositions.Contains(pos) = false && spacedMap.ContainsKey(pos) then
        if spacedMap[pos] = '.' then
            spacedMap[pos] <- 'O'
            // Spread out from here
            positionsToCheck <- positionsToCheck @ (NESWf |> Seq.map (fun dir -> pos + dir) |> Seq.toList)
        investigatedPositions <- investigatedPositions.Add(pos)
    //PrintSpacedMap()

PrintSpacedMap()

let containedTiles = 
    spacedMap 
    |> Seq.filter (fun pair -> pair.Value = '.')
    |> Seq.filter (fun pair -> pair.Key.X = Math.Round(pair.Key.X) && pair.Key.Y = Math.Round(pair.Key.Y))
    |> Seq.map (fun pair -> Vector2(int pair.Key.X, int pair.Key.Y))
    |> Seq.toList

// Print the map with the pipe network coloured in red
for y in { 0..lines.Length - 1 } do
    let line = lines[y]
    for x in { 0..line.Length - 1 } do
        let pos = Vector2(x,y)
        if network.Contains(pos) then
            Console.ForegroundColor <- ConsoleColor.Red
            printf "%c" pipeMap[pos]
        else if containedTiles |> List.contains pos then
            Console.ForegroundColor <- ConsoleColor.Green
            printf "%c" 'I'
        else
            Console.ForegroundColor <- defaultColour
            printf "%c" pipeMap[pos]
    printfn ""


let containedTileCount = 
    containedTiles
    |> Seq.length

printfn "[Part 2] Inner Tile Count %d" containedTileCount