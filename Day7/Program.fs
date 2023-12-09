open System

let lines = IO.File.ReadAllLines @"..\..\..\input.txt"

//let lines = [| "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" |]

type HandClass =
| FiveOfAKind = 7
| FourOfAKind = 6
| FullHouse = 5
| ThreeOAKind = 4
| TwoPair = 3
| OnePair = 2
| HighCard = 1

type Hand(hand:string, bid: int) =
    member val Hand = hand with get
    member val Bid = bid with get
    member this.Class = this.DetermineClass()
    member this.HexValue = this.CalculateHexValue()

    member val Rank = 0 with get, set

    override this.ToString() =
        sprintf "%s %d [%A] (Hex Value %s) -> Rank %d" this.Hand this.Bid this.Class (this.GetHexStringForHand()) this.Rank

    member private this.DetermineClass() =
        let counts =
            this.Hand
            |> Seq.groupBy (fun ch -> ch)
            |> Seq.map (fun (key, grp) -> Seq.length grp)
            |> Seq.toList

        if counts |> Seq.exists (fun i -> i = 5) then
            HandClass.FiveOfAKind
        else if counts |> Seq.exists (fun i -> i = 4) then
            HandClass.FourOfAKind
        else if counts |> Seq.exists (fun i -> i = 3)  then
            if counts |> Seq.exists (fun i -> i = 2) then HandClass.FullHouse else HandClass.ThreeOAKind
        else if counts |> Seq.filter (fun i -> i = 2) |> Seq.length = 2 then
            HandClass.TwoPair
        else if counts |> Seq.exists (fun i -> i = 2) then
            HandClass.OnePair
        else
            HandClass.HighCard

    member private this.CalculateHexValue() =
        Convert.ToInt32(this.GetHexStringForHand(), 16)

    member private this.GetHexStringForHand() =
        new String(this.Hand
            |> Seq.map this.GetHexCharForCard
            |> Seq.toArray)

    member private this.GetHexCharForCard(ch) =
        match ch with
        | 'A' -> 'E'
        | 'K' -> 'D'
        | 'Q' -> 'C'
        | 'J' -> 'B'
        | 'T' -> 'A'
        | _ -> ch

let hands =
    lines
    |> Seq.map (fun line -> line.Split(' ', StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (fun arr -> Hand(arr[0], int arr[1]))

let sortedHands =
    hands
    |> Seq.sortBy (fun hand -> hand.Class, hand.HexValue)
    |> Seq.toArray

seq { 0..sortedHands.Length - 1 }
    |> Seq.iter (fun i -> sortedHands[i].Rank <- i+1)

sortedHands
    |> Seq.iter (fun hand -> printfn "%A" hand)

let totalWinnings =
    sortedHands
    |> Seq.sumBy (fun hand -> hand.Rank * hand.Bid)

printfn "[Part 1]: Total Winnings: %d" totalWinnings

