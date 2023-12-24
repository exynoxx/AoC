module utils
open System.Collections.Generic

let toDict (tupls:('a*'b) seq) = 
    tupls |> Seq.map (fun (k,v)-> KeyValuePair.Create(k,v)) |> Dictionary

let dict (fkey: 'a -> 'g) (fval: 'b -> 'h) (seq:'a seq) = 
    seq |> Seq.map (fun e -> KeyValuePair.Create(fkey e, fval e)) |> Dictionary

let mapDict (dict:Dictionary<'a,'b>) (fkey: 'a -> 'g) (fval: 'b -> 'h) = 
    dict |> Seq.map (fun kv -> KeyValuePair.Create(fkey kv.Key, fval kv.Value)) |> Dictionary


let listmax (defaul:int) =
    function
    | [] -> defaul
    | x -> List.max x