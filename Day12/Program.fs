open System
open Utils
open System.Collections.Generic

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

lines <- "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1".Replace("\r\n", "\n").Split('\n')


let LineIsValid(line:string) =
    let pieces = line.Split(' ')
    let value = pieces[0]
    let requiredLengths = pieces[1].Split(',') |> Seq.map int |> Seq.toArray
    let faultyLengths = value.Split('.', StringSplitOptions.RemoveEmptyEntries) |> Seq.map (fun v -> v.Length )

    (Seq.compareWith Operators.compare faultyLengths requiredLengths) = 0

let FillComboIntoPlaceholders(line:char array, indices: int array, combo: char array) =
    let copy = line.Clone() :?> char array
    if combo.Length <> indices.Length then failwith "Invalid sizes, combo must be the same length as indices"
    seq { 0..indices.Length - 1}
        |> Seq.iter (fun n -> copy[indices[n]] <- combo[n])
    new String(copy)

let CalculatePossibilities(line) =
    printfn "%s" line

    let indices = seq { 0..line.Length - 1 } |> Seq.filter (fun i -> line[i] = '?') |> Seq.toArray

    let combos = Algorithm.GetPermutations [] indices.Length ['#';'.']
    
    //combos |> Seq.iter (fun combo -> printfn "  Combo: %A" combo)

    // Fill in the combos
    let validCombos = 
        combos
        |> Seq.map (fun combo -> 
            let filled = FillComboIntoPlaceholders(line.ToCharArray(), indices, combo)
            //printfn "%s = %A" filled (LineIsValid(filled))
            filled
        )
        |> Seq.filter LineIsValid
        |> Seq.length

    printfn "  Valid Combo Count: %A" validCombos

    validCombos

let part1Sum =
    lines 
    |> Seq.sumBy CalculatePossibilities

printfn "[Part 1]: Sum of all valid possibilities = %d" part1Sum

let Times5(list) = list @ list @ list @ list @ list

let CalculatePossibilitiesPart2(line:string) =

    let pieces = line.Split(' ')

    let values = String.Join('?', seq { pieces[0]; pieces[0]; pieces[0]; pieces[0]; pieces[0] } |> Seq.toList)
    let groupLengths = pieces[1].Split(',') |> Seq.map int |> Seq.toList |> Times5

    printfn "%s %s" values (String.Join(',', groupLengths))

    let rec GenerateSubCombinations(fix:string, rest:string) = seq {
        
        // Reached the bottom of it
        if rest.Length = 0 then
            yield fix

        //printfn "%s | %s" fix rest

        let nextWildCard = rest.IndexOf('?')

        if nextWildCard = -1 then
            yield fix + rest
        else
            let nonWildCards = if nextWildCard = 0 then "" else rest.Substring(0, nextWildCard)
            let left = fix + nonWildCards + "#"
            let right = fix + nonWildCards + "."

            if rest.Length = 1 then
                yield left
                yield right
            else
                for subresult in GenerateSubCombinations(left, rest.Substring(nextWildCard + 1)) do
                    yield subresult
                for subresult in GenerateSubCombinations(right, rest.Substring(nextWildCard + 1)) do
                    yield subresult
    }

    let combos = GenerateSubCombinations("", values)

    //combos |> Seq.iter (fun combo -> printfn "%s" combo)

    let validCombos = combos |> Seq.length
    
    printfn " Part 2 Valid Combo Count: %A" validCombos

    validCombos

let part2sum =
    lines
    |> Seq.sumBy CalculatePossibilitiesPart2

printfn "[Part 2]: Sum of all valid possibilities = %d" part2sum