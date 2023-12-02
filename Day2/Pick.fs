namespace Day2

open System

type Pick(colour, cubeCount: int) =
    member this.Colour = colour
    member this.Count = cubeCount

    new(line: string) =
        let pieces = line.Split(' ')
        let colourName = Char.ToUpper(pieces[1][0]).ToString() + pieces[1].Substring(1).ToLower()

        Pick(Enum.Parse(typedefof<Colour>, colourName) :?> Colour, Int32.Parse(pieces[0]))
