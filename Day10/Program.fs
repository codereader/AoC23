open System
open Utils
open Day10

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"
(*
lines <- "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ".Replace("\r\n", "\n").Split('\n')
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
        printfn "Expand from %A to %A" currentNode.Position newPos

        network <- network.Add(newPos)

        let newNode = NetworkNode(newPos)
        networkNodes <- networkNodes @ [newNode]
        currentNode.Forward <- Some(newNode)
        newNode.Back <- Some(currentNode)

        currentNode <- newNode
        validDirection <- TryFindValidDirection(currentNode)

networkNodes <- networkNodes @ [startNode]
ExpandNetworkFromPosition(startNode)

let rec FindForwardEnd(node:NetworkNode) =
    let next = node.Forward
    if next.IsSome then FindForwardEnd(next.Value) else node

startNode.Back <- Some(FindForwardEnd startNode)

lines
    |> Seq.iter (fun line -> printfn "%s" line)

printfn "%A" pipeMap
printfn "Start Position: %A" startPos

let mutable currentNode = startNode
while currentNode.Forward.IsSome do
    printfn "%A %A" currentNode.Position pipeMap[currentNode.Position]
    currentNode <- currentNode.Forward.Value

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

let rec CollectDistances(node:NetworkNode, forward: bool, distance: int) =
    let next = if forward then node.Forward else node.Back
    node.DistanceToStart <- System.Math.Min(node.DistanceToStart, distance)
    if next.IsSome && next.Value <> startNode then
        CollectDistances(next.Value, forward, distance + 1)

do CollectDistances(startNode, true, 0)
do CollectDistances(startNode, false, 0)

let farthestNode = networkNodes |> Seq.maxBy (fun node -> node.DistanceToStart)

printfn "[Part 1] Farthest Node at %A with distance %d" farthestNode.Position farthestNode.DistanceToStart
