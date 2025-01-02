module Utils

open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module List = 
    (* let cast<'T, 'U> (lst: 'T list) : 'U list =
        List.map (fun x -> x :?> 'U) lst *)
    let copy list = List.map id list
    let skipLast (list:'a list) = list[..list.Length-2]
    let wherei predicate lst =
        lst |> List.mapi (fun i x -> (i, x))
            |> List.filter (fun (i, x) -> predicate i x)
            |> List.map snd

module Array =
    let int (arr:string array) = arr |> Array.map int
    let int64 (arr:string array) = arr |> Array.map int64
    let any = Array.exists
    let exclude (idx:int) (arr:'a array) = [| for (i,e) in Array.indexed arr do if i <> idx then yield e |]
    let all_pairs (arr:'a array) =
        [| for i in 0 .. arr.Length - 1 do
            for j in i + 1 .. arr.Length - 1 do
                yield (arr[i], arr[j]) |]

module Seq = 
    let all_pairs (arr:'a array) =
        seq { 
            for i in 0 .. arr.Length - 1 do
                for j in i + 1 .. arr.Length - 1 do
                    yield (arr[i], arr[j])
        }

module Dictionary = 
    let merge (resolver: 'Value -> 'Value -> 'Value) (dicts: Dictionary<'Key, 'Value> array) =
        let result = Dictionary<'Key, 'Value>()
        
        for d in dicts do
            for kv in d do
                let existing = result.GetValueOrDefault(kv.Key)
                result[kv.Key] <- resolver existing kv.Value
        result
    let Items (x:Dictionary<'a,'b>) = x |> Seq.map (fun kv -> (kv.Key, kv.Value))
    

module String = 
    let Split (sep:string) (s:string) = s.Split sep

let ParseIntGrid (file:string) = 
    File.ReadAllLines(file)
    |> Array.map _.ToCharArray()
    |> Array.map (fun arr -> arr |> Array.map (fun c -> int c - int '0'))

let ParseGrid (file:string) = 
    File.ReadAllLines(file)
    |> Seq.map _.ToCharArray()
    |> Array.ofSeq

let ToIntTuple (a:string,b:string): int*int = (int a, int b)

let TupleOf (sep:string) (s:string) : string*string= 
    match s.Split(sep, StringSplitOptions.RemoveEmptyEntries) with
    | [|a;b|] -> (a, b)
    | _ -> failwith $"Not tuple {s}"

let IntTupleOf (sep:string) (s:string) : int*int = TupleOf sep s |> ToIntTuple

let IntTuple = IntTupleOf " "

// dict section
let Dict (tupls:('a*'b) seq) = 
    tupls |> Seq.map (fun (k,v)-> KeyValuePair.Create(k,v)) |> Dictionary

let MapDict (dict:Dictionary<'a,'b>) (fkey: 'a -> 'g) (fval: 'b -> 'h) = 
    dict |> Seq.map (fun kv -> KeyValuePair.Create(fkey kv.Key, fval kv.Value)) |> Dictionary


let inline (++) (a1,b1) (a2,b2) : 'a * 'b = (a1 + a2, b1 + b2)
let inline (--) (a1,b1) (a2,b2) : 'a * 'b = (a1 - a2, b1 - b2)

let MinByOrDefault (defaul:'b) f =
    function
    | [] -> defaul
    | x -> List.minBy f x

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

let regex (pattern:string) (input:string) = 
    let m = Regex.Match(input, pattern)
    if m.Success then
        Some (m.Captures |> Seq.map (fun x -> x.Value) |> Array.ofSeq)
    else 
        None