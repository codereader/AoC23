open System
open Day2

let lines = IO.File.ReadAllLines "..\..\..\input.txt"

// Game 1: 7 green, 4 blue, 3 red; 4 blue, 10 red, 1 green; 1 blue, 9 red

let games = lines |> Seq.map Game |> List.ofSeq

let possibleColours = 
    Map.empty.
        Add(Colour.Red, 12).
        Add(Colour.Green, 13).
        Add(Colour.Blue, 14)

let gameIsPossible (game: Game) =
    game.IsPossible possibleColours

printfn "%d games parsed" games.Length

let possibleGames = games |> Seq.filter gameIsPossible

printfn "%d games are possible" (Seq.length possibleGames)

let sumOfPossibleGames = possibleGames |> Seq.map (fun (game) -> game.Id) |> Seq.sum

printfn "[Part 1]: ID Sum of possible games: %d" sumOfPossibleGames