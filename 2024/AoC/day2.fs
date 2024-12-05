﻿module day2

open System.IO
open Utils


let f = File.ReadAllLines("Data/day2.txt") 
        |> Array.map (fun s -> s.Split(" ")) 
        |> Array.map (fun arr -> arr |> Array.map int)

let dec (a,b) = let d = a-b in d > 0 && d <= 3
let inc (a,b) = let d = a-b in d < 0 && d >= -3
let safe l = 
    l |> Array.pairwise |> Array.forall dec || 
    l |> Array.pairwise |> Array.forall inc

let pt1() = 
    let amount = f |> Array.where safe |> Array.length
    printfn $"pt1 {amount}"

let pt2() =
    let exclude idx arr = [| for (i,e) in Array.indexed arr do if i <> idx then yield e |]
    let safe2 (l:int array) = safe l || List.exists id [for i in 0 .. l.Length do yield safe (exclude i l)]

    let amount = f |> Array.where safe2 |> Array.length
    printfn $"pt2 {amount}"

pt1()
pt2()