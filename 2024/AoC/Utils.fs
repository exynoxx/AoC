module Utils

open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

module List = 
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

module String = 
    let Split (sep:string) (s:string) = s.Split sep

let ParseIntGrid (file:string) = 
    File.ReadAllLines(file)
    |> Array.map _.ToCharArray()
    |> Array.map (fun arr -> arr |> Array.map (fun c -> int c - int '0'))

let ParseGrid (file:string) = 
    File.ReadAllLines(file)
    |> Seq.map _.ToCharArray()
    |> Seq.map List.ofArray
    |> List.ofSeq

let ToIntTuple (tup:string*string) : int*int = 
    match tup with
    | (a,b) -> (int a, int b)
    | _ -> failwith $"No"

let TupleOf (sep:string) (s:string) : string*string= 
    match s.Split(sep, StringSplitOptions.RemoveEmptyEntries) with
    | [|a;b|] -> (a, b)
    | _ -> failwith $"Not tuple {s}"

let IntTupleOf (sep:string) (s:string) : int*int = 
    match s.Split(sep, StringSplitOptions.RemoveEmptyEntries) with
    | [|a;b|] -> (int a, int b)
    | _ -> failwith $"Not tuple {s}"

let IntTuple = IntTupleOf " "

// dict section
let Dict (tupls:('a*'b) seq) = 
    tupls |> Seq.map (fun (k,v)-> KeyValuePair.Create(k,v)) |> Dictionary

let MapDict (dict:Dictionary<'a,'b>) (fkey: 'a -> 'g) (fval: 'b -> 'h) = 
    dict |> Seq.map (fun kv -> KeyValuePair.Create(fkey kv.Key, fval kv.Value)) |> Dictionary


let inline (++) (a1,b1) (a2,b2) : 'a * 'b = (a1 + a2, b1 + b2)
let inline (--) (a1,b1) (a2,b2) : 'a * 'b = (a1 - a2, b1 - b2)

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

    member x.Items () = x |> Seq.map (fun kv -> (kv.Key, kv.Value))

let (?>) (x: 'a option) (defaultValue: 'a) : 'a = Option.defaultValue defaultValue x
let (|?) = defaultArg

type MinHeap<'T, 'G when 'G : comparison> (key: 'T -> 'G) =
    let mutable heap = List<'T>()
    
    let item i = key (heap[i])

    // Swap two elements in the heap
    let swap i j =
        let temp = heap.[i]
        heap.[i] <- heap.[j]
        heap.[j] <- temp

    // Bubble up to maintain the heap property
    let rec bubbleUp index =
        if index > 0 then
            let parentIndex = (index - 1) / 2
            if item index < item parentIndex then
                swap index parentIndex
                bubbleUp parentIndex

    // Bubble down to maintain the heap property
    let rec bubbleDown index =
        let leftChildIndex = 2 * index + 1
        let rightChildIndex = 2 * index + 2
        let size = heap.Count

        let smallest =
            if leftChildIndex < size && item leftChildIndex < item index then
                leftChildIndex
            else
                index

        let smallest = 
            if rightChildIndex < size && item rightChildIndex < item smallest then
                rightChildIndex
            else
                smallest

        if smallest <> index then
            swap index smallest
            bubbleDown smallest

    // Insert a new element into the heap
    member this.Insert(value: 'T) =
        (*heap.Add [|value|]
        bubbleUp (heap.Count - 1)*)
        ()

    // Remove and return the smallest element (root) from the heap
    member this.RemoveMin() =
        let minValue = heap.[0]
        let lastValue = heap[heap.Count - 1]
        heap <- heap.Slice(0, heap.Count - 1) // Remove the last element
        if heap.Count > 0 then
            heap.Insert(0, lastValue) // Put the last element at the root
            bubbleDown 0
        minValue

    // Peek the smallest element without removing it
    member this.Peek() = heap.[0]

    // Get the size of the heap
    member this.Size() = heap.Count

    // Check if the heap is empty
    member this.IsEmpty() = heap.Count = 0


