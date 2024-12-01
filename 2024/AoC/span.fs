module span

(*let Split (sep:string) (s:string) =
    let span = s.AsSpan()
    seq {
        for token in span.Split(sep) do
            yield span.Slice(token.Start.Value, token.End.Value)
    }

let IntTuple (sep:string) (s:string) : int*int = 
    match s.Split(" ", StringSplitOptions.RemoveEmptyEntries) with
    | [|a;b|] -> (int a, int b)
    | _ -> failwith $"Not tuple {s}"
*)