module Utils
open System

type Vector2(x,y) =
    interface IComparable with
        member this.CompareTo(obj) =
            let other = obj :?> Vector2
            ((this.X, this.Y) :> IComparable).CompareTo((other.X, other.Y))

    member val X = x with get,set
    member val Y = y with get,set

    override this.Equals(other) =
        let otherVec2 = other :?> Vector2
        otherVec2.X = this.X && otherVec2.Y = this.Y

    static member (+) (a: Vector2, b: Vector2) =
        Vector2(a.X + b.X, a.Y + b.Y)

    static member (-) (a: Vector2, b: Vector2) =
        Vector2(a.X - b.X, a.Y - b.Y)

    override this.GetHashCode() =
        System.HashCode.Combine(hash this.X, hash this.Y)

    override this.ToString() =
        sprintf "[%d %d]" this.X this.Y


type Vector2f(x: float,y: float) =
    interface IComparable with
        member this.CompareTo(obj) =
            let other = obj :?> Vector2f
            ((this.X, this.Y) :> IComparable).CompareTo((other.X, other.Y))

    member val X = x with get,set
    member val Y = y with get,set

    override this.Equals(other) =
        let otherVec2 = other :?> Vector2f
        otherVec2.X = this.X && otherVec2.Y = this.Y

    static member (+) (a: Vector2f, b: Vector2f) =
        Vector2f(a.X + b.X, a.Y + b.Y)

    static member (-) (a: Vector2f, b: Vector2f) =
        Vector2f(a.X - b.X, a.Y - b.Y)

    static member (/) (a: Vector2f, b: float) =
        Vector2f(a.X / b, a.Y / b)

    override this.GetHashCode() =
        System.HashCode.Combine(hash this.X, hash this.Y)

    override this.ToString() =
        sprintf "[%A %A]" this.X this.Y