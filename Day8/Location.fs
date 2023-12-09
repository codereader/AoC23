namespace Day8

type Location(input:string) =
    let Pieces = input.Split(" = ")
    let LR_Pieces = Pieces[1].Split(", ")
    member this.Name = Pieces[0]
    member this.Left = LR_Pieces[0].Replace("(", "").Replace(")", "")
    member this.Right = LR_Pieces[1].Replace("(", "").Replace(")", "")

    override this.ToString() =
        sprintf "%s (L: %s | R: %s)" this.Name this.Left this.Right

