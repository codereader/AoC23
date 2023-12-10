open System

// Real puzzle input
let  lines = IO.File.ReadAllLines @"..\..\..\input.txt"

let sequences =
    lines
    |> Seq.map (fun line -> line.Split(' '))
    |> Seq.map (fun strings -> strings |> Array.map Int32.Parse)
    |> Seq.toArray

let GetDifferences(sequence: int array) =
    { 1..sequence.Length - 1 }
        |> Seq.map (fun i -> sequence[i] - sequence[i-1])
        |> Seq.toArray

// The next extrapolated value for the given sequence
let rec GetFutureExtrapolatedValue(sequence: int array) =
    let differences = GetDifferences(sequence)

    let childInterpolation =
        if Seq.sum differences = 0 then 0 else GetFutureExtrapolatedValue differences

    sequence[sequence.Length - 1] + childInterpolation

// The interpolated value before the start of the given sequence
let rec GetPastExtrapolatedValue(sequence: int array) =
    let differences = GetDifferences(sequence)

    let childInterpolation =
        if Seq.sum differences = 0 then 0 else GetPastExtrapolatedValue differences

    sequence[0] - childInterpolation

let futureSum = sequences |> Seq.map GetFutureExtrapolatedValue |> Seq.sum

printfn "[Part 1]: Sum of all future extrapolated values: %d" futureSum

let pastSum = sequences |> Seq.map GetPastExtrapolatedValue |> Seq.sum

printfn "[Part 2]: Sum of all past extrapolated values: %d" pastSum