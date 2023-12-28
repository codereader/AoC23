namespace Day20

open System.Collections.Generic

[<AbstractClass>]
type Node(messageQueue: MessageQueue) = 
    member val MessageQueue = messageQueue with get
    member val OutputNodes = new List<Node>() with get
    member val OutputNodeNames = new List<string>() with get

    abstract member Name: string
    abstract member ReceiveImpulse: sender: string * highPulse: bool -> unit
