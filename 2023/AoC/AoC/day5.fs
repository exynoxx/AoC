module day5

open System.Text.RegularExpressions
open System.IO
open System.Collections
open System.Collections.Generic



let parseSeeds (s:string) = s.Split(' ') |> Seq.tail |> Seq.map int64

let (|SeedTo|_|) (line: string) =
    let regex = new Regex(@"to-(\w+) map:")
    let matchResult = regex.Match(line)

    if matchResult.Success then
        Some(matchResult.Groups.[1].Value)
    else
        None

type Range = int64*int64*int64

let parse () =
    let lines = File.ReadAllLines("day5.txt") |> List.ofSeq
    let seeds = parseSeeds lines.Head
    let blocks = Dictionary()

    let rec parseBlock i list : int * Range list = 
        if i >= lines.Length || lines[i] = "" then
            i+1, list
        else
            let [|a; b; c|] = lines[i].Split(' ')
            parseBlock (i+1) ((int64 a, int64 b,int64 c)::list)

    let rec parseAll i = 
        match lines[i] with
        | SeedTo t -> 
            let (idx, list) = parseBlock (i+1) []
            let sorted = list |> List.sortBy (fun (_,src,_) -> src)
            blocks.Add(blocks.Count,sorted)
            if t <> "location" then parseAll idx
            

    parseAll 2
    seeds, blocks



let withinRange range x = 
    let (_,src,len) = range
    src <= x && x < src + len

let translate range x = 
    let (dst,src,_) = range
    let offset = x - src
    dst+offset

let pt1() =
    let seeds, maps = parse ()

    let rec HandleLayer src i =
        if i = maps.Count then
            src
        else
            match maps[i] |> Seq.filter (fun range -> withinRange range src) |> List.ofSeq with 
            | [] -> HandleLayer src (i+1)
            | [range] -> HandleLayer (translate range src) (i+1)

    let smallest = seeds |> Seq.map (fun seed -> HandleLayer seed 0) |> Seq.min
    printfn $"{smallest}"

//too slow
let pt2() = 
    let seeds, maps = parse ()

    let rec takePairs = function
        | [] -> []
        | x::y::rest -> List.init (int y) (fun i -> x + int64 i) @ (takePairs rest)
            

    let rec HandleLayer src i =
        if i = maps.Count then
            src
        else
            match maps[i] |> Seq.filter (fun range -> withinRange range src) |> List.ofSeq with 
            | [] -> HandleLayer src (i+1)
            | [range] -> HandleLayer (translate range src) (i+1)

    let allseeds = takePairs (seeds |> List.ofSeq)
    let smallest = allseeds |> Seq.map (fun seed -> HandleLayer seed 0) |> Seq.min
    printfn $"{smallest}"


pt1()
(*let binarySearch input (numbers:(int64*int64*int64) list) =
    let withinRange src len x = 
        match () with
        | _ when x < src -> -1
        | _ when src <= x && x <= src + len -> 0
        | _ -> 1

    let rec search low high = 
        if low > high then
            None
        else
            let mid = (low + high) / 2
            let (dst, src, len) = numbers[mid]

            match withinRange src len input with
            | 0 -> Some (dst, src, len)
            | 1 -> search (mid + 1) high 
            | -1 -> search low (mid - 1)

    match search 0 (numbers.Length - 1) with
    | Some (dst,src,len) -> 
        let offset = input - src
        dst+offset
    | None -> input
*)