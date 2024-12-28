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
    let inline (@@) a b = Seq.append a b
    
    
    open System.Text.RegularExpressions

    let (>>) (input: string) (pattern: string) =
        match Regex(pattern).Matches(input) with
        | m when m.Count > 0 -> m.[0].Groups |> Seq.map (fun x -> x.Value) |> Array.ofSeq
        | _ -> [||]

    let (<<) (pattern: string) (input: string) = (>>) input pattern

    
    (*let input = "abc123"
    let pattern = "(\w)(\d+)"
    let captureGroups = input |> pattern*)

// captureGroups is ["abc", "123"]
    