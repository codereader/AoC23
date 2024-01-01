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

for node in nodes.Values do
    printfn "Node %s -> %s" node.Name (String.Join(", ", node.Connections |> Seq.map _.Name))

let rec CollectNodes(startNode: Node, set: HashSet<Node>) =
    startNode.Connections
        |> Seq.iter (fun childNode -> 
            if set.Contains(childNode) = false then
                ignore(set.Add(childNode))
                CollectNodes(childNode, set)
        )

let connectedNodes = new HashSet<Node>()
CollectNodes(nodes.Values |> Seq.head, connectedNodes)

printfn "Connected Nodes: %d of %d" connectedNodes.Count nodes.Count

let CheckEdgeTriplet(edge1: Edge, edge2: Edge, edge3: Edge) =
    
    DisconnectEdge(edge1)
    DisconnectEdge(edge2)
    DisconnectEdge(edge3)

    let firstGroup = new HashSet<Node>()
    CollectNodes(nodes.Values |> Seq.head, firstGroup)

    ConnectEdge(edge1)
    ConnectEdge(edge2)
    ConnectEdge(edge3)

    if firstGroup.Count <> nodes.Values.Count then
        Some((firstGroup.Count, nodes.Values.Count - firstGroup.Count))
    else
        None

let splitCount =
    seq {
    for i in { 0 .. edges.Length - 1 } do
        for j in { i+1 .. edges.Length - 1 } do
            for k in { j+1 .. edges.Length - 1 } do
                yield CheckEdgeTriplet(edges[i], edges[j], edges[k])
    }
    |> Seq.find _.IsSome

printfn "Group 1 = %d, Group 2 = %d" (fst splitCount.Value) (snd splitCount.Value)
printfn "[Part 1] Group counts multiplied = %d" ((fst splitCount.Value) * (snd splitCount.Value))

