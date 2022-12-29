namespace AoC

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