open System
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
// Test input
lines <- @"jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr".Replace("\r\n", "\n").Split('\n')
*)

let nodeNames =
    lines
    |> Seq.collect _.Replace(":", "").Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Seq.distinct
    |> Seq.toList

type Edge = { From: int; To: int }

type Node = { Name: string; Connections: HashSet<Node> }

let nodes = 
    nodeNames
    |> Seq.map (fun name -> { Name = name; Connections = new HashSet<Node>()} )
    |> Seq.map (fun node -> (node.Name, node))
    |> Map.ofSeq

let FindNodeIndex(name) =
    nodeNames |> Seq.findIndex (fun node -> node = name)

let edges =
    lines
    |> Seq.map _.Split(": ")
    |> Seq.collect (fun parts -> 
        let fromIndex = FindNodeIndex(parts[0])
        parts[1].Split(' ')
            |> Seq.map (fun toName -> FindNodeIndex(toName))
            |> Seq.map (fun toIndex -> { From = fromIndex; To = toIndex })
        )
    |> Seq.toArray
    
let ConnectEdge(edge: Edge) =
    let node1a = nodes[nodeNames[edge.From]]
    let node1b = nodes[nodeNames[edge.To]]
    ignore(node1a.Connections.Add(node1b))
    ignore(node1b.Connections.Add(node1a))

let DisconnectEdge(edge: Edge) =
    let node1a = nodes[nodeNames[edge.From]]
    let node1b = nodes[nodeNames[edge.To]]
    ignore(node1a.Connections.Remove(node1b))
    ignore(node1b.Connections.Remove(node1a))

edges |> Seq.iter ConnectEdge

printfn "%d Nodes: %A" nodes.Count (String.Join(", ", nodes.Values |> Seq.map _.Name))
printfn "%d Edges" edges.Length

let PrintEdge(edgeIndex) =
    sprintf "%s -> %s" nodeNames[edges[edgeIndex].From] nodeNames[edges[edgeIndex].To]

type ReachedNode = { Index: int; Edges: int Set }

let GetVisiblePath(path: int Set) =
    String.Join("|", path |> Seq.map (fun i -> sprintf "%s->%s" nodeNames[edges[i].From] nodeNames[edges[i].To]))

// Calculate reachabilities from one node to every other node, keep track of edges used
let CalculateReachabilities(startNodeIndex: int, cutEdges: int list) =
    let reachedNodes = new Dictionary<int, Set<int>>()

    let nodesToInvestigate = new Queue<ReachedNode>()
    nodesToInvestigate.Enqueue({ Index = startNodeIndex; Edges = Set.empty })
    ignore(reachedNodes.Add(startNodeIndex, Set.empty))

    while nodesToInvestigate.Count > 0 do
        let reachedNode = nodesToInvestigate.Dequeue()

        // Iterate over all edges we haven't used up to this point and are not marked as cut
        { 0 .. edges.Length - 1}
            |> Seq.filter (fun e -> reachedNode.Edges.Contains(e) = false)
            |> Seq.filter (fun e -> edges[e].From = reachedNode.Index || edges[e].To = reachedNode.Index)
            |> Seq.filter (fun e -> cutEdges |> Seq.contains e = false)
            |> Seq.iter (fun edgeIndex ->
                let edge = edges[edgeIndex]

                if reachedNodes.ContainsKey(edge.From) = false then
                    let newPath = reachedNode.Edges.Add(edgeIndex)
                    ignore(reachedNodes.Add(edge.From, newPath))
                    //printfn "Reached Node: %s through %s (%s)" nodeNames[edge.From] (GetVisiblePath(newPath)) (String.Join("|", newPath))
                    nodesToInvestigate.Enqueue({ Index = edge.From; Edges = newPath })

                if reachedNodes.ContainsKey(edge.To) = false then
                    let newPath = reachedNode.Edges.Add(edgeIndex)
                    ignore(reachedNodes.Add(edge.To, newPath))
                    //printfn "Reached Node: %s through %s (%s)" nodeNames[edge.To] (GetVisiblePath(newPath)) (String.Join("|", newPath))
                    nodesToInvestigate.Enqueue({ Index = edge.To; Edges = newPath })
            )

    reachedNodes

let FindEdgesToCut() =
    
    let mutable counts = Array.zeroCreate edges.Length

    let FindMostUsedEdge(paths: int Set seq) =
        // Weight the edges by the length of the set they're used in
        for set in paths do
            set |> Seq.iter (fun edgeIndex -> counts[edgeIndex] <- counts[edgeIndex] + 1)

    // Investigate the set of paths from various starting positions, three of them ought to be most prominent
    for i in { 0..10.. nodes.Count - 1 } do
        FindMostUsedEdge(CalculateReachabilities(i, List.empty) |> Seq.map _.Value)

    let mostUsedEdges =
        counts
        |> Seq.sortDescending
        |> Seq.take(3)
        |> Seq.map (fun maxValue -> (counts |> Seq.findIndex (fun v -> v = maxValue)))
        |> Seq.toList

    printfn "Most used edges: %A" mostUsedEdges
    mostUsedEdges


let cutEdges = FindEdgesToCut()
printfn "Edges to cut: %A" cutEdges
printfn "Edges to cut: %A" (cutEdges |> Seq.map PrintEdge)

let reachedNodes = CalculateReachabilities(0, cutEdges)

let group1 = reachedNodes.Count
let group2 = nodes.Count - reachedNodes.Count

printfn "Group 1 = %d, Group 2 = %d" group1 group2
printfn "[Part 1] Group counts multiplied = %d" (group1 * group2)

