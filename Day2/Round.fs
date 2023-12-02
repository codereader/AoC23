namespace Day2

type Round(picks: List<Pick>) =
    member this.Picks = picks

    // Construct a round from the given string "7 green, 4 blue, 3 red"
    new (line: string) =
        Round(line.Split(", ") |> Seq.map Pick |> List.ofSeq)

    member this.IsPossible(possibleColours : Map<Colour, int>) =
        let hasImpossiblePick = picks |> List.exists (fun pick -> possibleColours[pick.Colour] < pick.Count)
        not hasImpossiblePick

    member this.GetCountByColour(colour) =
        let pick = picks |> Seq.filter (fun pick -> pick.Colour = colour) |> Seq.tryHead
        if not pick.IsNone then pick.Value.Count else 0
