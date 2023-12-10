namespace Day10
open Utils

type NetworkNode(pos: Vector2) =
    member val Position = pos with get
    member val Forward:NetworkNode option = None with get,set
    member val Back:NetworkNode option = None with get,set
    member val DistanceToStart = System.Int32.MaxValue with get, set

