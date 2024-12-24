module day24

open System.IO
open Utils
open System.Collections.Generic

let eval_op l r= 
    function 
    | "XOR" -> l <> r 
    | "OR" -> l || r 
    | "AND" -> l && r

type GATE = {lhs: string; op:string; rhs:string; output:string}
type Node = 
    | Gate of GATE
    | Value of bool

let pt1() = 
    let raw_wires, init = 
        File.ReadAllLines("data/day24.txt")
        |> Array.where (fun s -> s <> "") 
        |> Array.partition (fun s -> s.Contains "->")

    let G = Dictionary<string,Node>()
    for value in init do
        let (l,r) = TupleOf ": " value
        G[l] <- Value (int r = 1)

    for w in raw_wires do
        let [|l;r|] = w.Split " -> "
        let [|lhs;op;rhs|] = l.Split " "
        
        G[r] <- Gate {lhs=lhs;op=op;rhs=rhs;output=r}

    let cache = Dictionary<string,bool>()
    let rec eval x = 
        if cache.ContainsKey x then 
            cache[x]
        else
            match G[x] with
            | Value v -> 
                cache[x] <- v
                v

            | Gate {lhs=l;op=o;rhs=r} -> 
                let val_l = eval l
                let val_r = eval r
                let v = 
                    match o with
                    | "XOR" -> val_l <> val_r
                    | "OR" -> val_l || val_r
                    | "AND" -> val_l && val_r
                cache[x] <- v
                v

    let output = 
        G.Keys 
        |> Seq.filter (fun output -> output.StartsWith "z")
        |> Array.ofSeq
        |> Array.sort
        |> Array.mapi (fun i e -> if eval e then 1UL <<< i else 0UL)
        |> Array.sum

    printfn "%i" output

pt1()