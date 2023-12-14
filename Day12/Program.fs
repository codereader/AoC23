open System
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(* // Test input
lines <- "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1".Replace("\r\n", "\n").Split('\n')
*)

(* // Old brute force algorithm used to solve part 1

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
*)

let mutable cache = new Dictionary<string, int64>()

let CalculatePossibilities(line:string) =

    let pieces = line.Split(' ')

    let stringWithPlaceholders = pieces[0]
    let requiredGroups = pieces[1].Split(',') |> Seq.map int |> Seq.toList

    // Compares the string against the full initial requirement "requiredGroups" in the parent function
    let LineIsFullyValid(line:string) =
        let pieces = line.Split(' ')
        let faultyLengths = pieces[0].Split('.', StringSplitOptions.RemoveEmptyEntries) |> Seq.map (fun v -> v.Length )
        (Seq.compareWith Operators.compare faultyLengths requiredGroups) = 0 // the full sequence has to match

    // Yields only completed groups in the given string as index pairs (startIndex, endIndex)
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

    let CompletedGroupsMightBeValid(completedGroups, requiredGroups) =
        Seq.forall2 (fun group requirement -> requirement = group) completedGroups requiredGroups

    let GetRemainingGroups(completedGroups: int array, requiredGroups: int list) =
        if completedGroups.Length = 0 then
            requiredGroups
        else
            let mismatchingIndex =
                seq { 0..requiredGroups.Length - 1 }
                |> Seq.tryFind (fun i -> i >= completedGroups.Length || requiredGroups[i] <> completedGroups[i])

            if mismatchingIndex.IsSome then requiredGroups[mismatchingIndex.Value ..] else []

    let CutOffCompletedGroups(fix: string, groupIndices: (int * int) array) =
        if groupIndices.Length > 0 then
            let lastHashIndex = snd groupIndices[groupIndices.Length - 1]
            (fix.Substring(0, lastHashIndex + 1), fix.Substring(lastHashIndex + 1)) // cut off the groups from the fixed string including the following dot
        else
            ("", fix)

    let GetGroupSizeForIndices(startIndex, endIndex) = endIndex - startIndex

    let GenerateKey(remainingString, remainingGroups) =
        let groupKey = String.Join(',', remainingGroups |> Seq.map string)
        remainingString + "|" + groupKey

    let rec GenerateSubCombinations(cutoff: string, fix:string, rest:string, requiredGroups: int list, level:int) = seq {
        
        // Find the wildcard in the remaining string
        let nextWildCard = rest.IndexOf('?')

        if nextWildCard = -1 then
            if LineIsFullyValid(cutoff + fix + rest) then yield 1L else yield 0L
        else
            let nonWildCards = if nextWildCard = 0 then "" else rest.Substring(0, nextWildCard)

            for substitute in seq { "#"; "." } do
                // Grab everything up to the next placeholder and fill in a substitute character
                let leftPart = fix + nonWildCards + substitute
                let remainingString = rest.Substring(nextWildCard + 1)

                // Check if this new left string contains any completed groups
                let completedGroupIndices = CompletedGroupsInLine leftPart |> Seq.toArray
                let completedGroupSizes = completedGroupIndices |> Array.map GetGroupSizeForIndices

                if CompletedGroupsMightBeValid(completedGroupSizes, requiredGroups) then
                    // Let's see how many groups we have left
                    let remainingGroups = GetRemainingGroups(completedGroupSizes, requiredGroups)
                    // And remove the completed group strings from the left part
                    let (newCutoff, newFix) = CutOffCompletedGroups(leftPart, completedGroupIndices)

                    if remainingGroups.Length = 0 && remainingString.IndexOf('#') <> -1 then
                        // hashes in the remaining strings and no more groups to distribute, there's no way
                        yield 0L
                    else if remainingGroups.Length = 0 && remainingString.IndexOf('?') <> -1 then
                        // No more groups to spare, check if what we have is valid
                        // if it isn't valid then this doesn't count as solution
                        // it it is valid then this counts as 1 solution since we can fill in dots only
                        if LineIsFullyValid(cutoff + newCutoff + newFix) then yield 1L else yield 0L
                    else
                        let cacheKey = GenerateKey(newFix + remainingString, remainingGroups)
                        let cacheHit, cachedResult = cache.TryGetValue(cacheKey)
                            
                        if cacheHit && remainingGroups.Length > 0 then 
                            yield cachedResult
                        else
                            // Investigate further => enter recursion
                            let result = GenerateSubCombinations(cutoff + newCutoff, newFix, remainingString, remainingGroups, level+1) |> Seq.sum
                            // Memoize this result, this cuts down computation time to manageable orders of magntitude
                            cache[cacheKey] <- result
                            yield result
    }

    GenerateSubCombinations("", "", stringWithPlaceholders, requiredGroups, 0) |> Seq.sum
    
printfn "Calculating Part 1..."

let stopWatch = new Stopwatch()
stopWatch.Start()

let part1Sum =
    lines 
    |> Seq.sumBy CalculatePossibilities

stopWatch.Stop()

// Part 1 Answer = 7857
printfn "[Part 1]: Sum of all valid possibilities = %d (this took %f seconds)" part1Sum stopWatch.Elapsed.TotalSeconds
printfn "[Part 1]: Correct answer was = 7857"

stopWatch.Start()

printfn "Calculating Part 2..."

let part2Sum =
    lines
    |> Seq.map (fun line ->
        let pieces = line.Split(' ')
        let values = String.Join('?', seq { pieces[0]; pieces[0]; pieces[0]; pieces[0]; pieces[0] })
        let requiredGroups = String.Join(',', seq { pieces[1]; pieces[1]; pieces[1]; pieces[1]; pieces[1] })
        values + " " + requiredGroups
        )
    |> Seq.sumBy CalculatePossibilities

stopWatch.Stop()

// Part 2 answer 28606137449920
printfn "[Part 2]: Sum of all valid possibilities = %d (this took %f seconds)" part2Sum stopWatch.Elapsed.TotalSeconds
printfn "[Part 2]: Correct answer was 28606137449920"