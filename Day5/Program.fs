open System

let lines = IO.File.ReadAllLines @"..\..\..\input.txt"

let seeds = lines[0].Substring(6).Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.map Int64.Parse

type Range(sourceStart: int64, destStart: int64, length: int64) =
    member this.Contains(input: int64) =
        input >= sourceStart && input <= sourceStart + length - 1L
    member this.Map(input: int64) =
        destStart + input - sourceStart

type Mapping(ranges: List<Range>) =
    member this.Map(input: int64) =
        let matchingRange = ranges |> Seq.tryFind (fun range -> range.Contains input)
        if matchingRange.IsSome then matchingRange.Value.Map input else input

let ParseRange (input: string) =
    let range = input.Trim().Split(' ') |> Array.map Int64.Parse
    Range(range[1], range[0], range[2])

let ParseMapping (input: string) = Mapping(input.Split("\n") |> Seq.map ParseRange |> Seq.toList)

let allMappings = seq { Input.SeedToSoil; Input.SoilToFertilizer; Input.FertilizerToWater;
    Input.WaterToLight; Input.LightToTemperature; Input.TemperatureToHumidity; Input.HumidityToLocation } |> Seq.map ParseMapping

let MapSeed (inputSeed: int64)=
    let mutable seed = inputSeed
    allMappings |> Seq.iter (fun mapping -> seed <- mapping.Map seed)
    seed

let minimumLocation = seeds |> Seq.map MapSeed |> Seq.min
printfn $"[Part 1] Minimum Location: {minimumLocation}" 