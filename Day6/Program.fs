// Time:        62     64     91     90
// Distance:   553   1010   1473   1074
let times = [
        (62, 553L)
        (64, 1010L)
        (91, 1473L)
        (90, 1074L)
    ]

let GetWaysToWin ((time: int, recordToBreak: int64)) =
    let waysToWin =
        seq { 0..time }
        |> Seq.filter (fun t -> (int64 t)*(int64 time) - (int64 t)*(int64 t) > recordToBreak)
        |> Seq.length
    waysToWin

let part1Product =
    times
    |> Seq.map GetWaysToWin
    |> Seq.fold (fun (product) item -> product*item) 1

printfn "[Part 1]: Product of all ways to win = %d" part1Product

let part2Solution = GetWaysToWin (62649190, 553101014731074L)
printfn "[Part 2]: Product of all ways to win = %d" part2Solution
