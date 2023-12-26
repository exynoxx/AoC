module utils
open System.Collections.Generic

let toDict (tupls:('a*'b) seq) = 
    tupls |> Seq.map (fun (k,v)-> KeyValuePair.Create(k,v)) |> Dictionary

let dict (fkey: 'a -> 'g) (fval: 'b -> 'h) (seq:'a seq) = 
    seq |> Seq.map (fun e -> KeyValuePair.Create(fkey e, fval e)) |> Dictionary

let mapDict (dict:Dictionary<'a,'b>) (fkey: 'a -> 'g) (fval: 'b -> 'h) = 
    dict |> Seq.map (fun kv -> KeyValuePair.Create(fkey kv.Key, fval kv.Value)) |> Dictionary

let inline (++) (a: 'a * 'b) (b: 'a * 'b) : 'a * 'b =
    let (a1, b1) = a
    let (a2, b2) = b
    (a1 + a2, b1 + b2)

let listmax (defaul:int) =
    function
    | [] -> defaul
    | x -> List.max x

let seqmax (defaul:int) seq =
    if seq |> Seq.exists (fun _ -> true) then
        Seq.max seq
    else 
        defaul

let seqmaxby (defaul:'a) (f:'a->int) (seq:'a seq) : 'a =
    if seq |> Seq.exists (fun _ -> true) then
        Seq.maxBy f seq
    else 
        defaul