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

Console.WriteLine "[Part 1]: Total of all numbers (first and last digit combined):"
Console.WriteLine total

//let allDigits = [| '0'; '1'; '2'; '3'; '4'; '5';'6'; '7';'8'; '9' |]
let allNumberWords = 
    [| 
        (0, "zero"); (1, "one"); (2, "two"); (3, "three"); (4, "four"); (5, "five"); (6, "six"); (7, "seven"); (8, "eight"); (9, "nine");
        (0, "0"); (1, "1"); (2, "2"); (3, "3"); (4, "4"); (5, "5"); (6, "6"); (7, "7"); (8, "8"); (9, "9")
    |]

let indexIsValid ((value: int, index: int)) = index >= 0
let takeValue ((value: int, index: int)) = value

let getLineNumberPart2 (line: String) =
    // First and last index of any word
    let first = allNumberWords |> Array.map (fun (value, word) -> (value, line.IndexOf word)) |> Array.filter indexIsValid |> Array.sortBy (fun (value, index) -> index) |> Array.head
    let last = allNumberWords |> Array.map (fun (value, word) -> (value, line.LastIndexOf word)) |> Array.filter indexIsValid |> Array.sortByDescending (fun (value, index) -> index) |> Array.head

    takeValue first * 10 + takeValue last

let allNumbersPart2 = lines |> Array.map getLineNumberPart2
let totalPart2 = allNumbersPart2 |> Array.sum

Console.WriteLine "[Part 2]: Total of all numbers including number words (first and last digit combined):"
Console.WriteLine totalPart2
