namespace Day20

open System.Collections.Generic
open System

type Conjunction(messageQueue, name) =
    inherit Node(messageQueue)

    member val InputSignals = new Dictionary<string, bool>() with get
    member val InputHiTimes = new Dictionary<string, int>() with get
    member val InputPeriodicity = new Dictionary<string, int>() with get

    member val RecordHiLo = false with get, set
    member val ButtonPresses = 0 with get, set

    override this.Name = name
    override this.ReceiveImpulse(sender: string, highPulse: bool) =

        if this.RecordHiLo && highPulse && this.InputSignals.Item(sender) <> highPulse then
            printfn "%d: Sig %s switch to %s" this.ButtonPresses sender (if highPulse then "HI" else "LO")
            
            let alreadyGotOneHi, value = this.InputHiTimes.TryGetValue(sender)

            if alreadyGotOneHi then
                let periodicity = this.ButtonPresses - value
                this.InputPeriodicity[sender] <- periodicity
                printfn "%d: Sig %s has periodicity %d" this.ButtonPresses sender periodicity
            else
                this.InputHiTimes[sender] <- this.ButtonPresses
            
        this.InputSignals.Item(sender) <- highPulse

        let pulseToSend = if this.InputSignals.Values |> Seq.forall (fun flag -> flag) then false else true

        for name in this.OutputNodeNames do
            this.MessageQueue.Push({ From = this.Name; To = name; IsHighPulse = pulseToSend })
        ()

    override this.ToString() =
        sprintf "conj %s (Inputs: %s)" this.Name (String.Join(", ", this.InputSignals.Keys))

// 238593356738827