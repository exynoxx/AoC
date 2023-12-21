module day8

open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic


let parseLine (s:string) = 
        let r = new Regex("(\w+)\s*=\s*\((\w+),\s*(\w+)\)")
        let m = r.Match s
        let groups = m.Groups
        let root = groups[1].Value
        let l = groups[2].Value
        let r = groups[3].Value
        KeyValuePair.Create(root, (l,r))

let parse () = 
    let lines = File.ReadAllLines("day8.txt") |> List.ofSeq
    let seed = lines[0].ToCharArray()
    let mappings = lines[2..] 
                    |> Seq.map parseLine 
                    //|> Seq.filter (fun kv -> not (kv.Key = fst kv.Value && kv.Key = snd kv.Value))
                    |> Dictionary
    
    (seed, mappings)

let pt1 () = 
    
    let seed, mappings = parse()
    let n = seed.Length

    let rec FindZ current i depth = 
        match current with
        | "ZZZ" -> depth
        | x -> 
            let (l,r) = mappings[x]
            match seed[i%n] with
            | 'L' -> FindZ l (i+1) (depth+1)
            | 'R' -> FindZ r (i+1) (depth+1)

    let answer = FindZ "AAA" 0 0
    printfn $"{answer}"


let pt2 () = 
    
    let seed, mappings = parse()
    let n = seed.Length
    let memory = Dictionary<string * int, string * int * int64>()

    let propagateNode node i = 
        let (l,r) = mappings[node]
        match seed[i] with
        | 'L' -> l
        | 'R' -> r

    let rec distToZ (node:string) i d = 
        match (node.EndsWith 'Z',d) with
        | (true, 0L) -> distToZ (propagateNode node i) ((i+1)%n) (d+1L)
        | (false,_) -> distToZ (propagateNode node i) ((i+1)%n) (d+1L)
        | (true, _) -> (node,i,d)
        
    let rec gcd a b =
        if b = 0L then a
        else gcd b (a % b)

    let lcm a b =
        if a = 0L || b = 0L then 0L
        else abs (a * b) / (gcd a b)

    let lcmList = function
        | [x] -> x
        | x :: xs -> List.fold lcm x xs

    let rec distToZCached (node:string,i,d) = 
        match memory.ContainsKey (node,i) with
        | true -> 
            let (next, nexti, offset) = memory[(node,i)]
            (next,nexti, d+offset)
        | false -> 
            let (next, j, offset) = distToZ node i 0L
            memory[(node,i)] <- (next, j, offset)
            (next, j, offset)


    let startNodes = mappings.Keys |> Seq.filter (fun k -> k.EndsWith 'A') |> List.ofSeq
    let firstZZNodes = startNodes |> List.map (fun x -> distToZ x 0 0L)

    //apparently. on a Z node. next Z node is oneself.
    let lcm = firstZZNodes |> Seq.map (fun (n,i,d) -> distToZCached (n,i,d) |> fun (_,_,d)->d) |> List.ofSeq
    let answer = lcmList lcm
    printfn $"{answer}"

pt1()
pt2()
