module span

(*open System

let SplitInt (sep:string) (s:string) =
    [
        for token in s.AsSpan().Split(sep) do
            yield (token.Start.Value, token.End.Value)
    ]
*)
(*let IntTuple (sep:string) (s:string) : int*int = 
    match s.Split(" ", StringSplitOptions.RemoveEmptyEntries) with
    | [|a;b|] -> (int a, int b)
    | _ -> failwith $"Not tuple {s}"
*)