namespace Day4

open System

type Card(id, winningNumbers: Set<int>, availableNumbers: List<int>) =
    member val Id = id
    member val WinningNumbers = winningNumbers
    member val AvailableNumbers = availableNumbers

    new(line: string) =
        if not (line.StartsWith("Card ")) then failwith "Line doesn't start with the Card keyword"

        let parts = line.Split(':')
        let id = Int32.Parse(parts[0].Substring("Card ".Length))

        // Split into winning and available numbers ("|")
        let numbers = parts[1].Trim().Split("|")

        let winningNumbers = numbers[0].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse |> Set.ofSeq
        let availableNumbers = numbers[1].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int32.Parse |> List.ofSeq

        Card(id, winningNumbers, availableNumbers)

    member this.Matches =
        this.AvailableNumbers |> Seq.filter (fun number -> this.WinningNumbers.Contains(number))

    member this.Value =
        let matches = this.Matches |> Seq.length
        if matches = 0 then 0 else (1 <<< matches - 1)

    member val Occurrences = 1 with get, set
