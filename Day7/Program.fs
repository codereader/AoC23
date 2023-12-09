open System

let lines = IO.File.ReadAllLines @"..\..\..\input.txt"

// Test input
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
    member this.Class = this.DetermineClass(this.Hand)

    // The numeric value representing the hand's strength
    member this.HexValue = this.CalculateHexValue(this.Hand)

    // Calculated rank after sorting, gets assigned from outside
    member val Rank = 0 with get, set

    override this.ToString() =
        sprintf "%s %d [%A] (Value %s, Rank %d)" this.Hand this.Bid this.Class (this.GetHexStringForHand(this.Hand)) this.Rank

    // Determine the hand's class based on the given string
    member this.DetermineClass(hand: seq<char>) =
        let counts =
            hand
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

    member this.CalculateHexValue(hand: seq<char>) =
        Convert.ToInt32(this.GetHexStringForHand(hand: seq<char>), 16)

    member private this.GetHexStringForHand(hand: seq<char>) =
        new String(hand
            |> Seq.map this.GetHexCharForCard
            |> Seq.toArray)

    abstract member GetHexCharForCard: char -> char
    
    default this.GetHexCharForCard(ch) =
        match ch with
        | 'A' -> 'E'
        | 'K' -> 'D'
        | 'Q' -> 'C'
        | 'J' -> 'B'
        | 'T' -> 'A'
        | _ -> ch

// Yield all combinations of the given set with N free slots
// Usage: e.g. GetCombinations [] 3 ['4';'3';'2']
// will create all 3-digit combinations, picking from the set [4,3,2]
let rec GetCombinations acc size set = seq {
  match size, set with
  | n, x::xs ->
      if n > 0 then yield! GetCombinations (x::acc) (n - 1) set
      if n >= 0 then yield! GetCombinations acc n xs
  | 0, [] -> yield acc |> Seq.toArray
  | _, [] -> () }

let AllCardsWithoutJoker = ['A';'K';'Q';'T';'9';'8';'7';'6';'5';'4';'3';'2']

type Part2Hand (hand:string, bid: int) =
    inherit Hand(hand, bid)

    member val HandWithJokersFilledIn = "" with get, set
    member val ClassWithJokersFilledIn = HandClass.HighCard with get, set
    member val ValueWithJokersFilledIn = 0 with get, set
    member val RankWithJokersFilledIn = 0 with get, set

    member this.DetermineBestValue() =
        Console.Write("Checking {0}...", this.Hand)

        let jokerSlots =
            seq { 0..this.Hand.Length-1 }
            |> Seq.filter (fun i -> this.Hand[i] = 'J')
            |> Seq.toArray

        if jokerSlots.Length > 0 then
            Console.Write("...Jokers at {0}...", String.Join(',', jokerSlots))

        let bestCombo =
            GetCombinations [] jokerSlots.Length AllCardsWithoutJoker
            // Distribute the combinations to the available slots and check that hand's strength
            |> Seq.map (fun combo -> this.FillJokers(jokerSlots, combo))
            |> Seq.map (fun combo -> (combo, (this.DetermineClass combo), (this.CalculateHexValue combo)))
            |> Seq.sortByDescending (fun (_,c,value) -> c, value)

        let (combo, cls, value) = bestCombo |> Seq.head

        this.HandWithJokersFilledIn <- new String(combo)
        this.ValueWithJokersFilledIn <- value
        this.ClassWithJokersFilledIn <- cls

        //combos
        //    |> Seq.iter (fun combo -> printfn "%A" combo)

        Console.WriteLine("found {0}", this.HandWithJokersFilledIn)

    member private this.FillJokers(indices, combination) =
        let chars = this.Hand.ToCharArray()
        for i in {0..indices.Length-1} do
            let index = indices[i]
            chars[index] <- combination[i]
        chars

    override this.GetHexCharForCard(ch) =
        match ch with
        | 'A' -> 'E'
        | 'K' -> 'D'
        | 'Q' -> 'C'
        | 'J' -> '1' // assign lower value for the Joker
        | 'T' -> 'A'
        | _ -> ch

    override this.ToString() =
        sprintf "%s [Optimised: %s, Rank: %d, Class: %A]" (base.ToString()) this.HandWithJokersFilledIn this.RankWithJokersFilledIn this.ClassWithJokersFilledIn


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

let part2Hands =
    lines
    |> Seq.map (fun line -> line.Split(' ', StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (fun arr -> Part2Hand(arr[0], int arr[1]))
    |> Seq.toArray

part2Hands
    |> Seq.iter (fun hand -> hand.DetermineBestValue())

let part2SortedHands =
    part2Hands
    |> Seq.sortBy (fun hand -> hand.ClassWithJokersFilledIn, hand.HexValue) // take the original value without jokers filled in
    |> Seq.toArray

seq { 0..part2SortedHands.Length - 1 }
    |> Seq.iter (fun i -> part2SortedHands[i].RankWithJokersFilledIn <- i+1)

part2SortedHands
    |> Seq.iter (fun hand -> printfn "%A" hand)

let part2TotalWinnings =
    part2SortedHands
    |> Seq.sumBy (fun hand -> (int64 hand.RankWithJokersFilledIn) * (int64 hand.Bid))

printfn "[Part 2]: Total Winnings: %d" part2TotalWinnings