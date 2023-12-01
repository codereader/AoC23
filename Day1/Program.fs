open System
open System.IO

let lines = File.ReadAllLines "..\..\..\input.txt"

let getLineNumber (line: String) =
    // Filter out all non-digits from the input string
    let digits = line.ToCharArray() |> Array.filter Char.IsDigit

    // Take the first digit
    let lastDigit = Array.last digits |> string |> int
    let firstDigit = Array.head digits |> string |> int

    firstDigit * 10 + lastDigit

let allNumbers = lines |> Array.map getLineNumber
let total = allNumbers |> Array.sum

Console.WriteLine "Total of all numbers (first and last digit combined):"
Console.WriteLine total
