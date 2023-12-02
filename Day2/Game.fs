namespace Day2

open System

type Game(id: int, rounds: seq<Day2.Round>) =
    member this.Id = id
    member this.Rounds = rounds

    new(line: string) =
        if not (line.StartsWith("Game ")) then failwith "Line doesn't start with the Game keyword"

        let parts = line.Split(':')
        let id = Int32.Parse(parts[0].Substring(5))

        // Split into rounds (";")

        let rounds = parts[1].Trim().Split("; ");

        Game(id, rounds |> Seq.map Round)

    member this.IsPossible(possibleColours) =
        let hasImpossibleRound = rounds |> Seq.exists (fun (round) -> round.IsPossible possibleColours = false)
        not hasImpossibleRound