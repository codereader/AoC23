namespace Day8

type Location(name, left, right) =
    member this.Name = name
    member this.Left = left
    member this.Right = right

    new (input:string) =
        let Pieces = input.Split(" = ")
        let LR_Pieces = Pieces[1].Split(", ")
        Location(Pieces[0], LR_Pieces[0].Replace("(", "").Replace(")", ""), LR_Pieces[1].Replace("(", "").Replace(")", ""))

    override this.ToString() =
        sprintf "%s (L: %s | R: %s)" this.Name this.Left this.Right

