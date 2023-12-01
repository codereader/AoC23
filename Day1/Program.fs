open System

// Part 1 is just using single digits, Part 2 is using both arrays
let allDigits = [| (0, "0"); (1, "1"); (2, "2"); (3, "3"); (4, "4"); (5, "5"); (6, "6"); (7, "7"); (8, "8"); (9, "9") |]
let allSpelledOutNumbers = [| (0, "zero"); (1, "one"); (2, "two"); (3, "three"); (4, "four"); (5, "five"); (6, "six"); (7, "seven"); (8, "eight"); (9, "nine"); |]

let lines = IO.File.ReadAllLines "..\..\..\input.txt"

let indexIsValid ((_: int, index: int)) = index >= 0

let getLineNumber (line: string, possibleWords) =
    // First and last index of any possible word
    let first =
      possibleWords
      |> Seq.map (fun (value, word: string) -> (value, line.IndexOf word))
      |> Seq.filter indexIsValid
      |> Seq.sortBy snd // sort by value
      |> Seq.head

    let last =
      possibleWords
      |> Seq.map (fun (value, word) -> (value, line.LastIndexOf word))
      |> Seq.filter indexIsValid
      |> Seq.sortByDescending snd // sort by value, descending
      |> Seq.head

    fst first * 10 + fst last

let totalPart1 = lines |> Seq.map (fun line -> (getLineNumber (line, allDigits))) |> Seq.sum

printfn "[Part 1]: Total of all numbers (first and last digit combined): %d" totalPart1

let totalPart2 = lines |> Seq.map (fun line -> (getLineNumber (line, Seq.concat [| allSpelledOutNumbers; allDigits |]))) |> Seq.sum

printfn "[Part 2]: Total of all numbers including number words (first and last digit combined): %d" totalPart2
