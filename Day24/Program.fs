open System
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
// Test input
lines <- @"19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3".Replace("\r\n", "\n").Split('\n')
*)

type Hailstone = { Pos: Vector3L; Velocity: Vector3L }

let hailstones = 
    lines
    |> Seq.map (fun line -> line.Split(" @ "))
    |> Seq.map (fun parts -> 
        let vec1 = parts[0].Split(", ")
        let vec2 = parts[1].Split(", ")
        { Pos = Vector3L(int64 vec1[0], int64 vec1[1], int64 vec1[2]); Velocity = Vector3L(int64 vec2[0], int64 vec2[1], int64 vec2[2]) }
        )
    |> Seq.toArray

printfn "%d hailstones: %A" hailstones.Length hailstones

let CrossingPoint(h1: Hailstone, h2: Hailstone) =
    let px1 = float h1.Pos.X
    let px2 = float h2.Pos.X
    let py1 = float h1.Pos.Y
    let py2 = float h2.Pos.Y
    let vx1 = float h1.Velocity.X
    let vx2 = float h2.Velocity.X
    let vy1 = float h1.Velocity.Y
    let vy2 = float h2.Velocity.Y

    let t2 = (py2/vy1 - py1/vy1 + px1/vx1 - px2/vx1) / (vx2/vx1 - vy2/vy1)

    if t2 > 0 then
        let x2 = px2 + t2 * vx2
        let y2 = py2 + t2 * vy2
        Some(Vector2f(x2, y2))
    else
        None

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

let indexPairs = seq {
    for i in { 0 .. hailstones.Length - 1 } do
        for j in { i+1 .. hailstones.Length - 1 } do
            yield (i,j)
    }

let rangeMin = 200000000000000.0
let rangeMax = 400000000000000.0

let numberOfCrossingsWithinArea = 
    indexPairs
    |> Seq.map (fun pair -> (hailstones[fst pair], hailstones[snd pair]))
    |> Seq.map (fun stones -> (CrossingPoint(fst stones, snd stones), CrossingPoint(snd stones, fst stones)))
    |> Seq.filter (fun pair -> (fst pair).IsSome && (snd pair).IsSome)
    |> Seq.map (fun pair -> (fst pair).Value)
    |> Seq.filter (fun pos -> pos.X >= rangeMin && pos.X <= rangeMax && pos.Y >= rangeMin && pos.Y <= rangeMax)
    |> Seq.length

printfn "[Part 1] %d" numberOfCrossingsWithinArea

#if false
for pair in indexPairs do
    printf "%A " pair
    let (h1, h2) = hailstones[fst pair], hailstones[snd pair]
    let pos = CrossingPoint(h1, h2)
    let pos2 = CrossingPoint(h2, h1)
    printf " Crossing Point: %A" pos
    printf " Crossing Point: %A" pos2

    if pos2.IsSome && pos.IsSome && pos.Value.X >= 7 && pos.Value.X <= 27 && pos.Value.Y >= 7 && pos.Value.Y <= 27 then printfn " YAY" else printfn ""
#endif


