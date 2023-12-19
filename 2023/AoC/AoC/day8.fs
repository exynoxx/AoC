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
    let memory = Dictionary<string * int, string * int * int>()

    let propagateNode node i = 
        let (l,r) = mappings[node]
        match seed[i] with
        | 'L' -> l
        | 'R' -> r

    let rec distToZ (node:string) i d = 
        match node.EndsWith 'Z' with
        | true -> (node,i,d)
        | false ->  distToZ (propagateNode node i) ((i+1)%n) (d+1)
        
    //zz node. step 1 forward(non-zz). recurse to next zz node. return zz node and dist
    let rec zzToNextZZ (node:string,i,d) = 
        if not (node.EndsWith 'Z') then
            failwith "invalid a"

        match memory.ContainsKey (node,i) with
        | true -> 
            let (next, nexti, offset) = memory[(node,i)]
            (next,nexti, d+offset)
        | false -> 
            let next = (propagateNode node i)
            let nexti = (i+1)%n
            let (zzNode, zzi, offset) = distToZ next nexti 1
            memory[(node,i)] <- (zzNode, zzi, offset)
            (zzNode,zzi, d+offset)

    let stepIfNotMax (node:string,i,d) max = 
        if d = max then
            (node,i,d)
        else
            zzToNextZZ (node,i,d)

    //step nodes than are behind furthest zz node
    let rec AllDistToZ (nodes: (string*int*int) list) = 
        let max = nodes |> Seq.map (fun (_,_,d) -> d) |> Seq.max
        let propagated = nodes |> List.map (fun (n,i,d) -> stepIfNotMax (n,i,d) max)

        printfn $"newMax {max}"
            
        if propagated |> Seq.forall (fun (_,_,d)-> d = max) then
            max
        else
            AllDistToZ propagated
     
    let kickStart (startNodes: string list) = 
        let firstZZNodes = startNodes |> List.map (fun x -> distToZ x 0 0)
        AllDistToZ firstZZNodes

    let startNodes = mappings.Keys |> Seq.filter (fun k -> k.EndsWith 'A') |> List.ofSeq
    let answer = kickStart startNodes
    printfn $"{answer}"

//pt1()
pt2()
