module ex

open System.Runtime.CompilerServices


[<Extension>]
type ArrayExtensions =
    [<Extension>]
    static member Nic(xs: 'a array) = Seq.sum xs