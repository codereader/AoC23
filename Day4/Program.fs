open System
open Day4

let lines = IO.File.ReadAllLines @"..\..\..\input.txt"

let cards = lines |> Seq.map Card |> Array.ofSeq
let totalCardValue = cards |> Seq.sumBy (fun card -> card.Value)

printfn "[Part 1]: Total value of all cards %d" totalCardValue

// Process the cards top down, counting up the occurrences

seq { 0..cards.Length - 1 }
    |> Seq.iter (fun index -> 
        let matchCount = Seq.length cards[index].Matches
        seq { 1..matchCount } |> Seq.iter (fun m -> cards[index + m].Occurrences <- cards[index + m].Occurrences + cards[index].Occurrences))

let totalNumberOfCards = cards |> Seq.sumBy (fun card -> card.Occurrences)

printfn "[Part 2]: Total number of all cards plus won cards %d" totalNumberOfCards
