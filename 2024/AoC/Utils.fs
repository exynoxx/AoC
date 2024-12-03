module Utils

open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

let ParseGrid (file:string) = 
    File.ReadAllLines(file)
    |> Seq.map _.ToCharArray()
    |> Array.ofSeq

let Dict (tupls:('a*'b) seq) = 
    tupls |> Seq.map (fun (k,v)-> KeyValuePair.Create(k,v)) |> Dictionary

let IntTuple (s:string) : int*int = 
    match s.Split(" ", StringSplitOptions.RemoveEmptyEntries) with
    | [|a;b|] -> (int a, int b)
    | _ -> failwith $"Not tuple {s}"

(*let Dict (fkey: 'a -> 'g) (fval: 'b -> 'h) (seq:'a seq) = 
    seq |> Seq.map (fun e -> KeyValuePair.Create(fkey e, fval e)) |> Dictionary
*)
let MapDict (dict:Dictionary<'a,'b>) (fkey: 'a -> 'g) (fval: 'b -> 'h) = 
    dict |> Seq.map (fun kv -> KeyValuePair.Create(fkey kv.Key, fval kv.Value)) |> Dictionary


let inline (++) (a: 'a * 'b) (b: 'a * 'b) : 'a * 'b =
    let (a1, b1) = a
    let (a2, b2) = b
    (a1 + a2, b1 + b2)

let MaxOrDefault (defaul:int) =
    function
    | [] -> defaul
    | x -> List.max x

let SeqMaxOrDefault (defaul:int) seq =
    if seq |> Seq.exists (fun _ -> true) then
        Seq.max seq
    else 
        defaul

let MaxByOrDefault (defaul:'a) (f:'a->int) (seq:'a seq) : 'a =
    if seq |> Seq.exists (fun _ -> true) then
        Seq.maxBy f seq
    else 
        defaul

let (|Regex|_|) (pattern:string) (input:string) = 
    let m = Regex.Match(input, pattern)
    if m.Success then
        Some m 
    else 
        None

type Dictionary<'Key, 'Value> with
    member x.Get (dict: Dictionary<'Key, 'Value>) (key: 'Key) : 'Value option =
        if dict.ContainsKey(key) then Some dict.[key]
        else None

let (?>) (x: 'a option) (defaultValue: 'a) : 'a = Option.defaultValue defaultValue x
let (|?) = defaultArg


type 'a ``[]`` with
    static member exclude idx (arr: 'a array) = [| for (i,e) in Array.indexed arr do if i <> idx then yield e |]
