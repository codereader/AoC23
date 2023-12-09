open System

// Real puzzle input
let lines = IO.File.ReadAllLines @"..\..\..\input.txt"

// Test input
//let lines = [| "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" |]

type HandType =
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
    member this.Type = Hand.DetermineType(this.Hand)

    // The numeric value representing the hand's strength, used for breaking ties
    member this.Value = this.CalculateHexValue(this.Hand)

    // Calculated rank after sorting, gets assigned from outside
    member val Rank = 0 with get, set

    override this.ToString() =
        sprintf "%s %d [%A] (Value %s, Rank %d)" this.Hand this.Bid this.Type (this.GetHexStringForHand(this.Hand)) this.Rank

    // Determine the hand's type based on the given string
    static member DetermineType(hand: seq<char>) =
        let counts =
            hand
            |> Seq.groupBy (fun ch -> ch)
            |> Seq.map (fun (key, grp) -> Seq.length grp)
            |> Seq.toList

        if counts |> Seq.exists (fun i -> i = 5) then
            HandType.FiveOfAKind
        else if counts |> Seq.exists (fun i -> i = 4) then
            HandType.FourOfAKind
        else if counts |> Seq.exists (fun i -> i = 3)  then
            if counts |> Seq.exists (fun i -> i = 2) then HandType.FullHouse else HandType.ThreeOAKind
        else if counts |> Seq.filter (fun i -> i = 2) |> Seq.length = 2 then
            HandType.TwoPair
        else if counts |> Seq.exists (fun i -> i = 2) then
            HandType.OnePair
        else
            HandType.HighCard

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

// The Part 2 Subtype of a game's hand is automatically determining the card's best possible type by filling in the jokers
type Part2Hand (hand:string, bid: int) =
    inherit Hand(hand, bid)

    member val HandWithJokersFilledIn = "" with get, set
    member val TypeWithJokersFilledIn = HandType.HighCard with get, set

    member this.FillJokers() =
        Console.Write("Checking {0}...", this.Hand)

        // Get the string positions the jokers are sitting at
        let jokerSlots =
            seq { 0..this.Hand.Length-1 }
            |> Seq.filter (fun i -> this.Hand[i] = 'J')
            |> Seq.toArray

        if jokerSlots.Length > 0 then
            Console.Write("...Jokers at ({0})...", String.Join(',', jokerSlots))

        // Use a brute-force algorithm to cover all possible combinations of joker replacements
        let bestCombo =
            GetCombinations [] jokerSlots.Length AllCardsWithoutJoker
            // Distribute the combinations to the available slots and check that hand's type
            |> Seq.map (fun combo -> this.FillJokers(jokerSlots, combo))
            |> Seq.map (fun combo -> (combo, (Hand.DetermineType combo)))
            |> Seq.sortByDescending (fun (_,c) -> c, this.Value)

        let combo = bestCombo |> Seq.head |> fst

        this.HandWithJokersFilledIn <- new String(combo)
        this.TypeWithJokersFilledIn <- Hand.DetermineType this.HandWithJokersFilledIn

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
        | 'J' -> '1' // assign lower value for the Joker, this is important for tie-breaking in the ranking
        | 'T' -> 'A'
        | _ -> ch

    override this.ToString() =
        sprintf "%s [Optimised: %s, Class: %A]" (base.ToString()) this.HandWithJokersFilledIn this.TypeWithJokersFilledIn

let hands =
    lines
    |> Seq.map (fun line -> line.Split(' ', StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (fun arr -> Hand(arr[0], int arr[1]))

let sortedHands =
    hands
    |> Seq.sortBy (fun hand -> hand.Type, hand.Value)
    |> Seq.toArray

seq { 0..sortedHands.Length - 1 }
    |> Seq.iter (fun i -> sortedHands[i].Rank <- i+1)

sortedHands
    |> Seq.iter (fun hand -> printfn "%A" hand)

let totalWinnings =
    sortedHands
    |> Seq.sumBy (fun hand -> hand.Rank * hand.Bid)

printfn "[Part 1]: Total Winnings: %d" totalWinnings

// Part 2: Re-parse the input, this time we're using a new Hand class type (to get the hang of inheritance)
let part2Hands =
    lines
    |> Seq.map (fun line -> line.Split(' ', StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (fun arr -> Part2Hand(arr[0], int arr[1]))
    |> Seq.toArray

part2Hands
    |> Seq.iter (fun hand -> hand.FillJokers())

// Careful when sorting the new hands, only the card type is using jokers, the value is the same
// "However, for the purpose of breaking ties between two hands of the same type, J is always treated as J, not the card it's pretending to be"
let part2SortedHands =
    part2Hands
    |> Seq.sortBy (fun hand -> hand.TypeWithJokersFilledIn, hand.Value) // use the original value to break ties
    |> Seq.toArray

// Assign rank
seq { 0..part2SortedHands.Length - 1 }
    |> Seq.iter (fun i -> part2SortedHands[i].Rank <- i+1)

part2SortedHands
    |> Seq.iter (fun hand -> printfn "%A" hand)

let part2TotalWinnings =
    part2SortedHands
    |> Seq.sumBy (fun hand -> hand.Rank * hand.Bid)

printfn "[Part 2]: Total Winnings: %d" part2TotalWinnings