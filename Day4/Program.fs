open System
open Day4

let lines = IO.File.ReadAllLines @"..\..\..\input.txt"

let cards = lines |> Seq.map Card
let totalCardValue = cards |> Seq.sumBy (fun card -> card.Value)

printfn "[Part 1]: Total value of all cards %d" totalCardValue