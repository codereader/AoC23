open System

let lines = IO.File.ReadAllLines @"..\..\..\input.txt"

//let seeds = "seeds: 79 14 55 13".Substring(6).Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int64.Parse |> Seq.toArray
let seeds = lines[0].Substring(6).Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int64.Parse |> Seq.toList

type Range(start: int64, length: int64) =
    member val Start = start
    member val Length = length
    member val End = start + length - 1L
    member this.Contains(point) =
        point >= this.Start && point <= this.End
    member this.Overlaps(other: Range) =
        this.Contains(other.Start) || this.Contains(other.End) || other.Contains(this.Start)
    member this.GetIntersection(other: Range) =
        if this.Overlaps(other) then
            Range(Math.Max(this.Start, other.Start), Math.Min(this.End, other.End) - Math.Max(this.Start, other.Start) + 1L)
        else
            Range(0, 0)
    override this.ToString() =
        sprintf "%d..%d" this.Start this.End

type Mapping(sourceStart: int64, destStart: int64, length: int64) =
    member val Source = Range(sourceStart, length)
    member val Destination = Range(destStart, length)
    member val Length = length

    member this.SourceContains(input: int64) = this.Source.Contains(input)
    member this.DestinationContains(input: int64) = this.Destination.Contains(input)

    member this.SourceOverlaps(input: Range) = this.Source.Overlaps(input)

    member this.OverlapsSourceOf(other: Mapping) =
        this.Destination.Overlaps(other.Source)

    member this.Map(input: int64) =
        destStart + input - sourceStart

    override this.ToString() =
        sprintf "From %d..%d to %d..%d" this.Source.Start this.Source.End this.Destination.Start this.Destination.End

type MappingCollection(ranges: List<Mapping>) =
    member val Mappings = ranges with get, set
    member this.Map(input: int64) =
        let matchingRange = ranges |> List.tryFind (fun range -> range.SourceContains input)
        if matchingRange.IsSome then matchingRange.Value.Map input else input

let ParseRange (input: string) =
    let range = input.Trim().Split(' ') |> Array.map Int64.Parse
    Mapping(range[1], range[0], range[2])

let ParseMapping (input: string) = MappingCollection(input.Split("\n") |> Seq.map ParseRange |> Seq.sortBy (fun mapping -> mapping.Source.Start) |> Seq.toList)

let allMappings = seq { Input.SeedToSoil; Input.SoilToFertilizer; Input.FertilizerToWater;
    Input.WaterToLight; Input.LightToTemperature; Input.TemperatureToHumidity; Input.HumidityToLocation } |> Seq.map ParseMapping |> Seq.toList

let MapSeed (inputSeed: int64) =
    let mutable seed = inputSeed
    allMappings |> List.iter (fun mapping -> seed <- mapping.Map seed)
    seed

let minimumLocation = seeds |> Seq.map MapSeed |> Seq.min
printfn $"[Part 1] Minimum Location (per seed): {minimumLocation}"

// Part 2: Find the lowest location interval
let part1SeedRanges =
    seeds
    |> Seq.map (fun seed -> Range(seed, 1))
    |> Seq.sortBy (fun range -> range.Start)

let seedRanges =
    seq { 0..seeds.Length / 2 - 1 }
    |> Seq.map (fun i -> (seeds[i*2], seeds[i*2+1]))
    |> Seq.sortByDescending fst
    |> Seq.map Range

let mutable inputSet = seedRanges |> Seq.sortBy (fun range -> range.Start)

printfn "Seed Ranges: %s" (String.Join("|", inputSet))

// Complete the mapping to fill the gaps with NOP mappings, not translating the values at all
let CreateDenseMapping(level: MappingCollection) =
    let result = MappingCollection([])
    
    let mutable mappings = level.Mappings |> Seq.sortBy (fun mapping -> mapping.Source.Start) |> Seq.toList
    let lastMapping = mappings[mappings.Length - 1]

    // Fill the gaps between the mappings with NOP mappings
    result.Mappings <- seq { 0..mappings.Length - 2 }
        |> Seq.collect (fun index -> 
            let nopStart = mappings[index].Source.End + 1L
            let nextStart = mappings[index+1].Source.Start
            seq { mappings[index]; Mapping(nopStart, nopStart, nextStart - nopStart) })
        |> Seq.filter (fun mapping -> mapping.Length > 0)
        |> Seq.toList

    result.Mappings <- result.Mappings @ [lastMapping]

    if (mappings.Head.Source.Start > 0) then
        // Create a range that is doing a NOP mapping
        result.Mappings <- Mapping(0, 0, mappings.Head.Source.Start) :: result.Mappings
    
    // A final NOP mapping after the last one
    result.Mappings <- result.Mappings @ [Mapping(mappings[mappings.Length - 1].Source.End + 1L, mappings[mappings.Length - 1].Source.End + 1L,
                               Int64.MaxValue - mappings[mappings.Length - 1].Source.End - 2L)]

    result

// Translate the given set of ranges to the next level using the mappings in the collection
let TranslateInputSet (level: MappingCollection, rangesToTransform) =
    // Fill the gaps of the incoming level mappings, the input outside the defined ranges will receive a NOP transform
    let denseMapping = CreateDenseMapping level

    //denseMapping.Mappings |> Seq.iter (fun mapping -> Console.Write $"{mapping.Source}|")
    //Console.WriteLine()

    // For each mapping find the ones overlapping the given range
    // Then calculate the intersection of the given range source range
    // Translate that into the target space of the next level
    rangesToTransform |> Seq.collect (fun range ->
        denseMapping.Mappings
        |> Seq.filter (fun mapping -> mapping.SourceOverlaps range)
        |> Seq.map (fun mapping ->
            // Calc the detailed intersection, translate this range to the next level
            let intersection = mapping.Source.GetIntersection(range)
            let mappedStart = mapping.Map(intersection.Start)
            let mappedEnd = mapping.Map(intersection.End)
            Range(mappedStart, mappedEnd - mappedStart + 1L)
            )
        |> Seq.sortBy (fun r -> r.Start) // ensure all ranges are sorted before forwarding them
        |> Seq.toList
        )

allMappings |> Seq.iter (fun level -> (inputSet <- TranslateInputSet(level, inputSet) |> Seq.toList))

let minimumSet = inputSet |> Seq.minBy (fun set -> set.Start)
printfn "[Part 2]: Minimum Location: %d" minimumSet.Start
