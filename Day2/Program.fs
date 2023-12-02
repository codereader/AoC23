open System
open Day2

// Game 1: 7 green, 4 blue, 3 red; 4 blue, 10 red, 1 green; 1 blue, 9 red
let lines = IO.File.ReadAllLines "..\..\..\input.txt"

let games = lines |> Seq.map Game |> List.ofSeq

let possibleColours = 
    Map.empty.
        Add(Colour.Red, 12).
        Add(Colour.Green, 13).
        Add(Colour.Blue, 14)

printfn "%d games parsed" games.Length
printfn "%d games are possible" (Seq.length (games |> Seq.filter (fun game -> game.IsPossible possibleColours)))

let sumOfPossibleGames = games |> Seq.filter (fun game -> game.IsPossible possibleColours) |> Seq.map (fun game -> game.Id) |> Seq.sum

printfn "[Part 1]: ID sum of possible games: %d" sumOfPossibleGames

// Part 2: Get the power of all games, and sum it up
let totalPower = games |> Seq.sumBy (fun game -> game.Power)
printfn "[Part 2]: Power sum of games: %d" totalPower

