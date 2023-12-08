// Time:        62     64     91     90
// Distance:   553   1010   1473   1074
let times = [
        (62, 553 )
        (64, 1010)
        (91, 1473)
        (90, 1074)
    ]

let totalProduct =
    times
    |> Seq.map (fun (time, recordToBreak) ->
        let waysToWin =
            seq { 0..time }
            |> Seq.filter (fun t -> t*time-t*t > recordToBreak)
            |> Seq.length
        waysToWin
    )
    |> Seq.fold (fun (product) item -> product*item) 1

printfn "[Part 1]: Product of all ways to win = %d" totalProduct