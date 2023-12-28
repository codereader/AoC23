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

printfn "Nodes: %s" (String.Join("\n", nodes.Values))
printfn "Conjunctions: %d" conjunctions.Count
printfn "Conjunctions: %A" conjunctions.Values

let nodeConnectingToRx =
    nodes.Values
    |> Seq.filter (fun node -> node.OutputNodeNames.Contains("rx"))
    |> Seq.cast<Conjunction>
    |> Seq.head

// Keep track of the inputs needing to be in HI at the same time
nodeConnectingToRx.RecordHiLo <- true
let nodesNeedingToGoHigh = nodeConnectingToRx.InputSignals

printfn "Conjunction connecting to RX: %A" nodeConnectingToRx
printfn "Nodes needed to switch to HI at the same time: %A" nodeConnectingToRx

let broadcaster = nodes.Values |> Seq.find (fun node -> node.Name = "broadcaster")

let mutable count = 0
while nodeConnectingToRx.InputPeriodicity.Count < nodeConnectingToRx.InputSignals.Count do
    count <- count + 1
    nodeConnectingToRx.ButtonPresses <- count
    messageQueue.Push({ From = button.Name; To = broadcaster.Name; IsHighPulse = false })

    while messageQueue.HasMessages do
        let message = messageQueue.Queue.Dequeue()

        let found, node = nodes.TryGetValue(message.To)

        if found then
            node.ReceiveImpulse(message.From, message.IsHighPulse)

    if count = 1000 then
        printfn "[Part 1]: Pulses recorded: %d (Low: %d, High: %d)" messageQueue.MessageCount messageQueue.LowPulses messageQueue.HighPulses
        printfn "[Part 1]: Pulse Product: %d" messageQueue.PulseProduct // 681194780

printfn "[Part 2]: Periodicities %s" (String.Join(", ", nodeConnectingToRx.InputPeriodicity.Values))

let mutable product = 1L
nodeConnectingToRx.InputPeriodicity.Values |> Seq.iter (fun p -> product <- product * int64 p)
printfn "[Part 2]: Product of all periodicities = %d" product // 238593356738827
