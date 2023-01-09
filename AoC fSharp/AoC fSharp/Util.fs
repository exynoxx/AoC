namespace AoC

open System.Collections.Generic

module Util =
    let Pairs (source: seq<_>) =
        seq {
            use iter = source.GetEnumerator()
            iter.MoveNext()|>ignore
            let mutable a = iter.Current
            while iter.MoveNext() do
                let b = iter.Current
                yield (a, b)
                a <- b

        }
    type Dictionary<'K,'V> with
        member x.GetValOrDefault v def =
            if x.ContainsKey v then x[v] else def
    type Seq<'T> =
        static member product = Seq.reduce (*)
        
    let inline (++) a b = (fst a + fst b, snd a + snd b)
    