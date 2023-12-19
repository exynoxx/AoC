module day5

open System.Text.RegularExpressions
open System.IO
open System.Collections
open System.Collections.Generic


let ToDict (keySelector: 'a -> 'key) (valueSelector: 'a -> 'value) seq = seq |> Seq.map (fun e -> KeyValuePair.Create (keySelector e, valueSelector e)) |> Dictionary

let parseSeeds (s:string) = s.Split(' ') |> Seq.tail |> Seq.map int64

let (|SeedTo|_|) (line: string) =
    let regex = new Regex(@"to-(\w+) map:")
    let matchResult = regex.Match(line)

    if matchResult.Success then
        Some(matchResult.Groups.[1].Value)
    else
        None

type Mapping = int64*int64*int64*int64
type Interval = int64*int64

let parse () =
    let lines = File.ReadAllLines("day5.txt") |> List.ofSeq
    let seeds = parseSeeds lines.Head
    let blocks = Dictionary()

    let rec parseBlock i list = 
        if i >= lines.Length || lines[i] = "" then
            i+1, list
        else
            let [|a; b; c|] = lines[i].Split(' ')
            parseBlock (i+1) ((int64 a, int64 b,int64 c)::list)

    let rec parseAll i = 
        match lines[i] with
        | SeedTo t -> 
            let (idx, list) = parseBlock (i+1) []
            let pointToPoint = list |> List.map (fun (dst,src,len) -> Mapping (src,src+len,dst,dst+len)) |> List.sortBy (fun (src,_,_,_) -> src)
            blocks.Add(blocks.Count,pointToPoint)
            if t <> "location" then parseAll idx
            

    parseAll 2
    seeds, blocks

let pt1() =
    let seeds, maps = parse ()

    let withinRange (src,srcstop,_,_) x = src <= x && x < srcstop

    let translate (src,_,dst,_) x = 
        let offset = x - src
        dst+offset

    let rec HandleLayer src i =
        if i = maps.Count then
            src
        else
            match maps[i] |> Seq.filter (fun range -> withinRange range src) |> List.ofSeq with 
            | [] -> HandleLayer src (i+1)
            | [range] -> HandleLayer (translate range src) (i+1)

    let smallest = seeds |> Seq.map (fun seed -> HandleLayer seed 0) |> Seq.min
    printfn $"{smallest}"


//not working
let pt2() = 

    let seeds, maps = parse ()

    let rec takePairs = function
        | [] -> []
        | x::y::rest ->  Interval (x, x + y) :: (takePairs rest)
       
    
    let findOverlapping (a,b) (map:Mapping list) : Mapping list = 
        seq {
            for (x,y,x1,y1) in map do
                if max a x <= min b y then
                    yield (x,y,x1,y1)
        } |> List.ofSeq


    let rec translate (input:Interval list) (output: Immutable.ImmutableHashSet<Interval>) (map:Mapping list) = 
        match input with
        | [] -> output |> List.ofSeq
        | (a,b)::remains -> 
            match findOverlapping (a,b) map with
            | _ when a >= b -> translate remains output map
            | [] -> 
                translate remains (output.Add (a,b)) map
            | overlapping -> 
                let mutable extraInput = []
                let mutable extraOutput = []

                for (x,y,xdst,ydst) in overlapping do
                    let overlap = (max a x, min b y)
                    let xdiff = (fst overlap)-x
                    let ydiff = (snd overlap)-y
                    let newStuff = [(a,x-1L); (b+1L,y)]
                    extraInput <- List.append extraInput newStuff
                    extraOutput <- (xdst+xdiff, ydst+ydiff)::extraOutput 

                let newInput = List.append remains extraInput
                translate newInput (output.Union extraInput) map


    let rec translateAll (intervals:Interval list) i = 
        if i = maps.Count then
            intervals
        else
            let map = maps[i]
            let dstIntervals = translate intervals (Immutable.ImmutableHashSet.Create()) map
            translateAll dstIntervals (i+1)


    let allseeds = takePairs (seeds |> List.ofSeq)
    let alldestionations = translateAll allseeds 0
    let smallest = alldestionations |> Seq.map fst |> Seq.min
    printfn $"{smallest}"


pt1()
pt2()