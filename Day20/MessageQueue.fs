namespace Day20

open System.Collections.Generic

type MessageQueue() =
    member val Queue = new Queue<Message>()
    member val MessageCount = 0 with get, set
    member val HighPulses = 0 with get, set
    member val LowPulses = 0 with get, set

    member this.HasMessages = this.Queue.Count > 0

    member this.Push(message) =
        //printfn "%s -%s-> %s" message.From (if message.IsHighPulse then "high" else "low") message.To
        this.Queue.Enqueue(message)
        this.MessageCount <- this.MessageCount + 1

        if message.IsHighPulse then
            this.HighPulses <- this.HighPulses + 1
        else
            this.LowPulses <- this.LowPulses + 1

    member this.PulseProduct = 
        int64 this.LowPulses * int64 this.HighPulses
