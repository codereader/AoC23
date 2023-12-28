namespace Day20

type Broadcaster(messageQueue, name) =
    inherit Node(messageQueue)

    override this.Name = name
    override this.ReceiveImpulse(sender: string, highPulse: bool) =
        for name in this.OutputNodeNames do
            this.MessageQueue.Push({ From = this.Name; To = name; IsHighPulse = highPulse })
