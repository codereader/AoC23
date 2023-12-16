open System
open System.Collections.Generic

// Real puzzle input
let mutable lines = IO.File.ReadAllLines @"..\..\..\input.txt"

// Test input
//lines <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7".Replace("\r\n", "\n").Split('\n')

let sequence = (lines |> Seq.head).Split(',', StringSplitOptions.RemoveEmptyEntries)

let CalculateHash(input:string) =
    let mutable hash = 0
    input |> Seq.iter (fun ch -> hash <- ((hash + int ch) * 17) % 256)
    hash

printfn "Hash of 'HASH' = %d" (CalculateHash "HASH")

let sequenceHash = 
    sequence
    |> Seq.map CalculateHash
    |> Seq.sum

// Correct answer is 512950
printfn "[Part 1] Intialization Sequence Hash = %d"  sequenceHash

type Lens = { Label:string; mutable FocalLength: int }

let mutable boxes: List<Lens> list = List.init 256 (fun i -> (new List<Lens>()))

let AddToBox(boxId, lensLabel, focalLength) =
    //printfn "Box %d: adding %s with focal length %d" boxId lensLabel focalLength
    let mutable lensExisted = false
    for lens in boxes[boxId] do
        if lens.Label = lensLabel then
            lens.FocalLength <- focalLength
            lensExisted <- true

    if not lensExisted then
        boxes[boxId].Add({ Label = lensLabel; FocalLength = focalLength })

let RemoveFromBox(boxId, lensLabel) =
    //printfn "Box %d: removing %s" boxId lensLabel
    ignore (boxes[boxId].RemoveAll(fun l -> l.Label = lensLabel))

let PerformInstruction(instruction:string) =
    let assignmentIndex = instruction.IndexOf('=')

    if assignmentIndex <> -1 then
        let lensLabel = instruction.Substring(0, assignmentIndex)
        let boxId = CalculateHash (lensLabel)
        let focalLength = int (instruction.Substring(assignmentIndex + 1))
        AddToBox(boxId, lensLabel, focalLength)
    else
        let removalIndex = instruction.IndexOf('-')
        let lensLabel = instruction.Substring(0, removalIndex)
        let boxId = CalculateHash (lensLabel)
        RemoveFromBox(boxId, lensLabel)

sequence
    |> Seq.iter PerformInstruction

let CombinedFocusingPower(boxIndex, box: List<Lens>) =
    let boxStrength = (boxIndex + 1)
    box |> Seq.mapi (fun lensIndex lens -> boxStrength * (lensIndex + 1) * lens.FocalLength) |> Seq.sum

let focusingPower = 
    boxes
    |> Seq.mapi (fun boxIndex box -> CombinedFocusingPower(boxIndex, box))
    |> Seq.sum

// Correct answer is 247153
printfn "[Part 2]: Focusing Power = %d" focusingPower