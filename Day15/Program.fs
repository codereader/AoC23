open System

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

