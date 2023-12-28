namespace Day20

type Button(messageQueue) =
    inherit Node(messageQueue)

    override this.Name = "button"
    override this.ReceiveImpulse(sender: string, highPulse: bool) =
        for name in this.OutputNodeNames do
            this.MessageQueue.Push({ From = this.Name; To = name; IsHighPulse = highPulse })

    