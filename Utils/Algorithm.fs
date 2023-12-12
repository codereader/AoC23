module Algorithm

// Yield all combinations of the given set with N free slots
// Usage: e.g. GetCombinations [] 3 ['4';'3';'2']
// will create all 3-digit combinations, picking from the set [4,3,2]
// Only creates one combination for each set of picks, no permutations
let rec GetCombinations acc size set = seq {
  match size, set with
  | n, x::xs ->
      if n > 0 then yield! GetCombinations (x::acc) (n - 1) set
      if n >= 0 then yield! GetCombinations acc n xs
  | 0, [] -> yield acc |> Seq.toArray
  | _, [] -> () }

// Yield all combinations including permutations
let rec GetPermutations acc size set = seq {
  match size, set with
  | n, head::tail ->
      if n > 0 then 
        for item in set do
            yield! GetPermutations (item::acc) (n - 1) set
      if n = 0 then
        yield! GetPermutations acc n tail
  | 0, [] -> yield acc |> Seq.toArray
  | _, [] -> () }
