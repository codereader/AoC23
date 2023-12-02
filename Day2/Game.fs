namespace Day2

open System

type Game(id: int, rounds: List<Day2.Round>) =
    member this.Id = id
    member this.Rounds = rounds

    new(line: string) =
        if not (line.StartsWith("Game ")) then failwith "Line doesn't start with the Game keyword"

        let parts = line.Split(':')
        let id = Int32.Parse(parts[0].Substring(5))

        // Split into rounds (";")
        let rounds = parts[1].Trim().Split("; ") |> Seq.map Round |> List.ofSeq

        Game(id, rounds)

    member this.IsPossible(possibleColours) =
        let hasImpossibleRound = rounds |> Seq.exists (fun (round) -> round.IsPossible possibleColours = false)
        not hasImpossibleRound

    member this.Power =
        let maxRed = rounds |> Seq.map (fun (round) -> round.GetCountByColour Colour.Red) |> Seq.max
        let maxBlue = rounds |> Seq.map (fun (round) -> round.GetCountByColour Colour.Blue) |> Seq.max
        let maxGreen = rounds |> Seq.map (fun (round) -> round.GetCountByColour Colour.Green) |> Seq.max

        maxRed * maxBlue * maxGreen