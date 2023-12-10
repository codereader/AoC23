open System

// Real puzzle input
let  lines = IO.File.ReadAllLines @"..\..\..\input.txt"

let sequences =
    lines
    |> Seq.map (fun line -> line.Split(' '))
    |> Seq.map (fun strings -> strings |> Array.map Int32.Parse)
    |> Seq.toArray

// The next interpolated value for the given sequence
let rec GetFutureExtrapolatedValue(sequence: int array) =
    let mutable differenceSum = 0
    let differences = 
        { 1..sequence.Length - 1 }
        |> Seq.map (fun i -> sequence[i] - sequence[i-1])
        |> Seq.map (fun i ->
            differenceSum <- differenceSum + i
            i)
        |> Seq.toArray

    let childInterpolation =
        if differenceSum = 0 then 0 else GetFutureExtrapolatedValue differences

    sequence[sequence.Length - 1] + childInterpolation

let extrapolations = sequences |> Seq.map GetFutureExtrapolatedValue
let extrapolationSum = extrapolations |> Seq.sum

printfn "[Part 1]: Sum of all future extrapolated values: %d" extrapolationSum

let rec GetPastExtrapolatedValue(sequence: int array) =
    let mutable differenceSum = 0
    let differences = 
        { 1..sequence.Length - 1 }
        |> Seq.map (fun i -> sequence[i] - sequence[i-1])
        |> Seq.map (fun i ->
            differenceSum <- differenceSum + i
            i)
        |> Seq.toArray

    let childInterpolation =
        if differenceSum = 0 then 0 else GetPastExtrapolatedValue differences

    sequence[0] - childInterpolation

let pastExtrapolations = sequences |> Seq.map GetPastExtrapolatedValue
let pastExtrapolationSum = pastExtrapolations |> Seq.sum

printfn "[Part 2]: Sum of all past extrapolated values: %d" pastExtrapolationSum