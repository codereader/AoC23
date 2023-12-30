open System
open Utils
open System.Collections.Generic
open System.Diagnostics

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

(*
// Test input
lines <- @"1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9".Replace("\r\n", "\n").Split('\n')
*)

type Brick(line) =
    interface IComparable with
        member this.CompareTo(obj) =
            let other = obj :?> Brick
            this.Id.CompareTo(other.Id)

    member val Blocks = Brick.GetBlocks(line) with get
    member val Name = Brick.GetNextName() with get
    member val Id = Brick.GetNextId() with get

    member this.LowestBlocks =
        let minimumZ = this.Blocks |> Seq.map (fun (block:Vector3) -> block.Z) |> Seq.min
        this.Blocks |> Seq.filter (fun b -> b.Z = minimumZ) |> Seq.toList

    member this.HighestBlocks =
        let maximumZ = this.Blocks |> Seq.map (fun (block:Vector3) -> block.Z) |> Seq.max
        this.Blocks |> Seq.filter (fun b -> b.Z = maximumZ) |> Seq.toList

    member val BricksBelow = new HashSet<Brick>() with get
    member val BricksAbove = new HashSet<Brick>() with get

    member val BricksThatWouldFall: HashSet<Brick> = null with get, set

    static member private GetBlocks(line:string) =
        let vectors = line.Split('~')
        let a = vectors[0].Split(',') |> Array.map (fun s -> int s)
        let b = vectors[1].Split(',') |> Array.map (fun s -> int s)
        let startPos = Vector3(a[0], a[1], a[2])
        let endPos = Vector3(b[0], b[1], b[2])

        let brickVector = endPos - startPos
        let length = (brickVector.X + brickVector.Y + brickVector.Z)

        if length > 0 then
            let unitLength = brickVector / length;

            seq { 0 .. length }
                |> Seq.map (fun i -> startPos + unitLength * i)
                |> Seq.toList
        else
            [startPos]

    override this.ToString() = this.Name

    static member val NextId = 1 with get, set
    static member val NextName = 'A' with get, set

    static member private GetNextName() =
        let name = new String(Brick.NextName, 1)
        Brick.NextName <- char (int Brick.NextName + 1)
        name
        
    static member private GetNextId() =
        let id = Brick.NextId
        Brick.NextId <- Brick.NextId + 1
        id

let grid = new Dictionary<Vector3, Brick>()

let bricks =
    lines
    |> Seq.map Brick
    |> Seq.sortBy (fun brick -> brick.LowestBlocks.Head.Z)
    |> Seq.toArray

// Fill in the grid
bricks |> Seq.iter (fun brick -> brick.Blocks |> Seq.iter (fun block -> grid[block] <- brick))

let GetOtherBrickBelow(grid: Dictionary<Vector3, Brick>, brick: Brick, pos: Vector3) =
    let hasBrick, existing = grid.TryGetValue(pos - Vector3(0,0,1))
    if hasBrick && existing.Id <> brick.Id then Some(existing) else None

let CanMoveDownward(grid: Dictionary<Vector3, Brick>, brick: Brick) =
    brick.Blocks |> Seq.forall (fun pos -> pos.Z > 1 && GetOtherBrickBelow(grid, brick, pos).IsNone)

let MoveDownward(grid, brick) =
    while CanMoveDownward(grid, brick) do
        brick.Blocks |> Seq.iter (fun block ->
            ignore (grid.Remove(block))
            block.Z <- block.Z - 1
            grid[block] <- brick
        )
    ()

bricks |> Seq.iter (fun brick -> MoveDownward(grid, brick))

//grid |> Seq.iter (fun kvp -> printfn "%A %A" kvp.Key kvp.Value.Name)

bricks |> Seq.iter (fun brick ->
        for block in brick.Blocks do
            let brickBelow = GetOtherBrickBelow(grid, brick, block)
            if brickBelow.IsSome then
                ignore(brick.BricksBelow.Add(brickBelow.Value))
                ignore(brickBelow.Value.BricksAbove.Add(brick))
    )

let bricksToDisintegrate =
    bricks
    |> Seq.filter (fun brick -> brick.BricksAbove.Count = 0 || brick.BricksAbove |> Seq.forall (fun above -> above.BricksBelow.Count > 1))
    |> Seq.toArray

printfn "[Part 1]: Bricks to disintegrate: %d %A" bricksToDisintegrate.Length bricksToDisintegrate // 475

let rec FindBricksThatWouldFall(brick: Brick, unstable: HashSet<Brick>) =
    let result = new HashSet<Brick>()

    ignore(unstable.Add(brick))

    for above in brick.BricksAbove do
        if above.BricksBelow |> Seq.forall (fun b -> unstable.Contains(b)) then
            ignore(result.Add(above))
            ignore(unstable.Add(above))

            for recursiveBrick in FindBricksThatWouldFall(above, unstable) do 
                ignore(result.Add(recursiveBrick))

    result

let totalBricksThatWouldFall =
    bricks
    |> Seq.map (fun brick -> FindBricksThatWouldFall(brick, (new HashSet<Brick>())))
    |> Seq.sumBy (fun list -> list.Count)

printfn "[Part 2]: Bricks that would fall: %d" totalBricksThatWouldFall // 79144