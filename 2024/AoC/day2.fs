module day2

open Utils
open System.IO


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

    let excludei i seq =
        seq
        |> Array.mapi (fun idx element -> (idx, element))
        |> Array.filter (fun (idx, _) -> idx <> i)
        |> Array.map snd  

    let safe2 (l:int array) = List.exists (fun i -> l |> excludei i |> safe) [0 .. l.Length]

    let amount = f |> Array.where safe2 |> Array.length
    printfn $"pt2 {amount}"

pt1()
pt2()
