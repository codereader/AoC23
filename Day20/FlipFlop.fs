namespace Day20

type FlipFlop(messageQueue, name) =
    inherit Node(messageQueue)

    member val State = false with get, set

    override this.Name = name
    override this.ReceiveImpulse(sender: string, highPulse: bool) =
        if not highPulse then
            this.State <- not this.State
            for name in this.OutputNodeNames do
                this.MessageQueue.Push({ From = this.Name; To = name; IsHighPulse = this.State })
        ()

    override this.ToString() =
        sprintf "flip %s (State: %s)" this.Name (if this.State then "ON" else "off")
