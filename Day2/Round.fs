namespace Day2

type Round(picks: List<Pick>) =
    member this.Picks = picks

    // Construct a round from the given string "7 green, 4 blue, 3 red"
    new (line: string) =
        let picks = line.Split(", ")

        Round(picks |> Seq.map Pick |> List.ofSeq)

    member this.IsPossible(possibleColours : Map<Colour, int>) =
        let hasImpossiblePick = picks |> List.exists (fun (pick) -> possibleColours[pick.Colour] < pick.Count)
        not hasImpossiblePick