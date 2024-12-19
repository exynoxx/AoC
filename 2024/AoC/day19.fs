module day19

open System
open System.Collections.Generic
open System.IO
open Utils

let f = File.ReadAllLines("data/day19.txt")
let towels = f[0] |> String.Split ", "

let startsWith (s:string) (prefix: string) (offset:int) = 
    if prefix.Length - 1 + offset >= s.Length then 
        false
    else
        seq {0 .. prefix.Length - 1} |> Seq.exists (fun i -> s.[offset+i] <> prefix.[i]) |> not


let pt1 () =

    let rec possible (s:string) = 
        function
        | offset when offset >= s.Length -> true 
        | offset -> towels |> Array.exists (fun t -> startsWith s t offset && possible s (offset+t.Length))

    let result = f[2..] |> Array.filter (fun pattern -> possible pattern 0) |> Array.length
    printfn "%i" result

let pt2() = 
    let possibilities (s:string) = 

        let mem = Dictionary<int,int64>()
        let rec inner = 
            function 
            | offset when offset >= s.Length -> 1L
            | offset when mem.ContainsKey offset -> mem[offset]
            | offset -> 
                let matches = 
                    towels 
                    |> Array.filter (fun t -> startsWith s t offset)
                    |> Array.sumBy (fun t -> inner (offset+t.Length))

                mem[offset] <- matches
                matches
        inner 0

    printfn "%i" (f[2..] |> Array.sumBy possibilities)

pt1()
pt2()