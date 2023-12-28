namespace Day20

open System.Collections.Generic
open System

type Conjunction(messageQueue, name) =
    inherit Node(messageQueue)

    member val InputSignals = new Dictionary<string, bool>() with get

    override this.Name = name
    override this.ReceiveImpulse(sender: string, highPulse: bool) =
        this.InputSignals.Item(sender) <- highPulse

        let pulseToSend = if this.InputSignals.Values |> Seq.forall (fun flag -> flag) then false else true

        for name in this.OutputNodeNames do
            this.MessageQueue.Push({ From = this.Name; To = name; IsHighPulse = pulseToSend })
        ()

    override this.ToString() =
        sprintf "conj %s (Inputs: %s)" this.Name (String.Join(", ", this.InputSignals.Keys))
