open System
open Day20
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
// Test input
lines <- @"broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output".Replace("\r\n", "\n").Split('\n')
*)

let messageQueue = new MessageQueue()

let GetOutputNames(line:string) = 
    let rightPart = line.Split(" -> ")[1]
    rightPart.Split(", ")

let CreateNode(line:string) =
    if line.StartsWith("broadcaster") then
        let node = Broadcaster(messageQueue, "broadcaster")
        node.OutputNodeNames.AddRange(GetOutputNames(line))
        node :> Node
    else if line[0] = '%' then
        let node = FlipFlop(messageQueue, line.TrimStart('%').Split(" -> ")[0])
        node.OutputNodeNames.AddRange(GetOutputNames(line))
        node :> Node
    else if line[0] = '&' then
        let node = Conjunction(messageQueue, line.TrimStart('&').Split(" -> ")[0])
        node.OutputNodeNames.AddRange(GetOutputNames(line))
        node :> Node
    else
        failwith "???"

let button = Button(messageQueue)
button.OutputNodeNames.Add("broadcaster")

let nodes = 
    lines
    |> Seq.map (fun line ->
        let node = CreateNode(line)
        (node.Name, node)
        )
    |> Seq.append (seq { (button.Name, button :> Node) })
    |> Map.ofSeq

let conjunctions =
    nodes.Values
    |> Seq.filter (fun x -> x :? Conjunction)
    |> Seq.cast<Conjunction>
    |> Seq.map (fun con -> (con.Name, con))
    |> Map.ofSeq

nodes.Values
    |> Seq.iter (fun node ->
        for name in node.OutputNodeNames do
            let found, con = conjunctions.TryGetValue(name)

            if found && con.InputSignals.ContainsKey(node.Name) = false then
                con.InputSignals.Add(node.Name, false)
    )

printfn "Nodes: %s" (String.Join(", ", nodes.Values))
printfn "Conjunctions: %d" conjunctions.Count
printfn "Conjunctions: %A" conjunctions.Values

let broadcaster = nodes.Values |> Seq.find (fun node -> node.Name = "broadcaster")

for _ in { 1..1000 } do
    messageQueue.Push({ From = button.Name; To = broadcaster.Name; IsHighPulse = false })

    while messageQueue.HasMessages do
        let message = messageQueue.Queue.Dequeue()

        let found, node = nodes.TryGetValue(message.To)

        if found then
            node.ReceiveImpulse(message.From, message.IsHighPulse)

printfn "[Part 1]: Pulses recorded: %d (Low: %d, High: %d)" messageQueue.MessageCount messageQueue.LowPulses messageQueue.HighPulses
printfn "[Part 1]: Pulse Product: %d" messageQueue.PulseProduct // 681194780

messageQueue.Reset()

