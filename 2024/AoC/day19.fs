module day19

open System
open System.Collections.Generic
open System.IO
open Utils

let f = File.ReadAllLines("data/day19.txt")
let towels = f[0] |> String.Split ", "


let rec possible_pt1 (s:ReadOnlySpan<char>) = 
    if s.Length = 0 then
        true
    else 
        let mutable found = false
        for towel in towels do 
            if not found && s.StartsWith towel && possible_pt1 (s.Slice towel.Length) then 
                found <- true
        found

let pt1 () =
    let mutable result = f[2..] 
                        |> Array.filter (fun pattern -> possible_pt1 (pattern.AsSpan())) 
                        |> Array.length
    printfn "%i" result

let rec possible_pt2 (mem:Dictionary<int,int64>) offset (s:ReadOnlySpan<char>) = 
    if s.Length = 0 then
        1L
    elif mem.ContainsKey offset then 
        mem[offset]
    else 
        let mutable matches = 0L
        for towel in towels do 
            if s.StartsWith towel then 
                matches <- matches + possible_pt2 mem (offset+towel.Length) (s.Slice towel.Length)

        mem[offset] <- matches
        matches

//350870 to low
let pt2() = printfn "%i" (f[2..] |> Array.sumBy (fun pattern -> possible_pt2 (Dictionary<int,int64>()) 0 (pattern.AsSpan())) )

pt1()
pt2()