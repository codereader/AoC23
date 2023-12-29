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

    static member (*) (a: Vector2, b: int) =
        Vector2(a.X * b, a.Y * b)

    static member (-) (a: Vector2, b: Vector2) =
        Vector2(a.X - b.X, a.Y - b.Y)

    override this.GetHashCode() =
        System.HashCode.Combine(hash this.X, hash this.Y)

    override this.ToString() =
        sprintf "[%d %d]" this.X this.Y

    static member Up = Vector2(0,-1)
    static member Right = Vector2(1,0)
    static member Down = Vector2(0,1)
    static member Left = Vector2(-1,0)

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

type Vector3(x,y,z) =
    interface IComparable with
        member this.CompareTo(obj) =
            let other = obj :?> Vector3
            ((this.X, this.Y, this.Z) :> IComparable).CompareTo((other.X, other.Y, other.Z))

    member val X = x with get,set
    member val Y = y with get,set
    member val Z = z with get,set

    override this.Equals(other) =
        let otherVec = other :?> Vector3
        otherVec.X = this.X && otherVec.Y = this.Y && otherVec.Z = this.Z

    static member (+) (a: Vector3, b: Vector3) =
        Vector3(a.X + b.X, a.Y + b.Y, a.Z + b.Z)

    static member (*) (a: Vector3, b: int) =
        Vector3(a.X * b, a.Y * b, a.Z * b)

    static member (/) (a: Vector3, b: int) =
        Vector3(a.X / b, a.Y / b, a.Z / b)

    static member (-) (a: Vector3, b: Vector3) =
        Vector3(a.X - b.X, a.Y - b.Y, a.Z - b.Z)

    override this.GetHashCode() =
        System.HashCode.Combine(hash this.X, hash this.Y, hash this.Z)

    override this.ToString() =
        sprintf "[%d %d %d]" this.X this.Y this.Z
