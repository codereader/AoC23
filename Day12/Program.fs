open System
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

//lines <- ".?.?.?#????.??#?... 1,2".Replace("\r\n", "\n").Split('\n');

(*
lines <- "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1".Replace("\r\n", "\n").Split('\n')
*)

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

(*
Part 1 Answer = 7857

let part1Sum =
    lines 
    |> Seq.sumBy CalculatePossibilities

printfn "[Part 1]: Sum of all valid possibilities = %d" part1Sum
*)

let Times5(list) = list @ list @ list @ list @ list

let mutable cache = new Dictionary<string, int64>()

let CalculatePossibilitiesPart2(line:string) =

    let pieces = line.Split(' ')

    let values = String.Join('?', seq { pieces[0]; pieces[0]; pieces[0]; pieces[0]; pieces[0] } |> Seq.toList)
    let requiredGroups = pieces[1].Split(',') |> Seq.map int |> Seq.toList |> Times5

    printfn "%s %s" values (String.Join(',', requiredGroups))

    // Yields only completed groups
    let CompletedGroupsInLine(line:string) = seq {
        let mutable hashStartIndex = line.IndexOf('#')

        while hashStartIndex <> -1 do
            let hashEndIndex = line.IndexOf('.', hashStartIndex)

            if hashEndIndex <> -1 then
                yield (hashStartIndex, hashEndIndex)
                hashStartIndex <- line.IndexOf('#', hashEndIndex)
            else
                hashStartIndex <- -1
        }

    let LineIsFullyValid(line:string) =
        let pieces = line.Split(' ')
        let faultyLengths = pieces[0].Split('.', StringSplitOptions.RemoveEmptyEntries) |> Seq.map (fun v -> v.Length )

        (Seq.compareWith Operators.compare faultyLengths requiredGroups) = 0

    let CompletedGroupsAreValid(completedGroups, requiredGroups) =
        Seq.forall2 (fun group requirement ->
            requirement = group) completedGroups requiredGroups

    let GetRemainingGroups(completedGroups: int array, requiredGroups: int list) =
        if completedGroups.Length = 0 then
            requiredGroups
        else
            let mismatchingIndex =
                seq { 0..requiredGroups.Length - 1 }
                |> Seq.tryFind (fun i -> i >= completedGroups.Length || requiredGroups[i] <> completedGroups[i])

            if mismatchingIndex.IsSome then requiredGroups[mismatchingIndex.Value ..] else []

    let SplitRemainingString(fix: string, groupIndices: (int * int) array) =
        if groupIndices.Length > 0 then
            let lastHashIndex = snd groupIndices[groupIndices.Length - 1]
            (fix.Substring(0, lastHashIndex), fix.Substring(lastHashIndex)) // cut off the groups from the fixed string
        else
            ("", fix)

    let GetGroupSizeForIndices(startIndex, endIndex) = endIndex - startIndex

    let GenerateKey(remainingString, remainingGroups) =
        let groupKey = String.Join(',', remainingGroups |> Seq.map string)
        remainingString + "|" + groupKey

    let rec GenerateSubCombinations(cutoff: string, fix:string, rest:string, requiredGroups: int list, level:int) = seq {
        
        let cacheHit, cachedResult = cache.TryGetValue(GenerateKey(fix + rest, requiredGroups))

        if cacheHit && requiredGroups.Length > 0 then
            cachedResult
        else
            let nextWildCard = rest.IndexOf('?')

            if nextWildCard = -1 then
                if LineIsFullyValid(cutoff + fix + rest) then 1L else 0L
            else
                let nonWildCards = if nextWildCard = 0 then "" else rest.Substring(0, nextWildCard)

                for substitute in seq { "#"; "." } do
                    let left = fix + nonWildCards + substitute
                    let remainingString = rest.Substring(nextWildCard + 1)

                    let completedLeftGroupIndices = CompletedGroupsInLine left |> Seq.toArray
                    let completedLeftGroupSizes = completedLeftGroupIndices |> Array.map GetGroupSizeForIndices

                    if CompletedGroupsAreValid(completedLeftGroupSizes, requiredGroups) then
                        let remainingGroups = GetRemainingGroups(completedLeftGroupSizes, requiredGroups)
                        let (newCutoff, newFix) = SplitRemainingString(left, completedLeftGroupIndices)

                        if remainingGroups.Length = 0 then
                            if remainingString.IndexOf('#') <> -1 then 0L else 1L // only 1 possibility left provided there are no hashes in the remaining string
                        else
                            let result = GenerateSubCombinations(cutoff + newCutoff, newFix, remainingString, remainingGroups, level+1) |> Seq.sum
                            cache[GenerateKey(newFix + remainingString, remainingGroups)] <- result
                            result
    }

    let stopWatch = new Stopwatch()
    stopWatch.Start()

    let combos = GenerateSubCombinations("", "", values, requiredGroups, 0) |> Seq.sum

    //combos |> Seq.iter (fun combo -> printfn "%s" combo)

    let validCombos = combos

    stopWatch.Stop()
    
    printfn " => Combo Count: %A in %f seconds" validCombos (stopWatch.Elapsed.TotalSeconds)

    validCombos

let part2sum =
    lines
    |> Seq.sumBy CalculatePossibilitiesPart2

printfn "[Part 2]: Sum of all valid possibilities = %d" part2sum