module day20

open Utils
open System.Collections.Generic
open DataStructures
open System

let G = ParseGrid("data/day20.txt")

let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let item (i,j) =
    if i >= 0 && i < G.Length && j >= 0 && j < G[0].Length then 
        G[i][j]
    else 
        '#'

let get_element x = 
    seq {
        for i in 0..G.Length - 1 do
            for j in 0..G[0].Length - 1 do
                if G[i][j] = x then
                    yield (i,j) 
    } |> Seq.exactlyOne

let S = get_element 'S'
let E = get_element 'E'

let adj = [|(0,1);(0,-1);(1,0);(-1,0)|]
let neighboors u = adj |> Array.map (fun dv -> dv++u)

let DFS s e = 
    let path = Dictionary<int*int,int>()
    let rec inner x i = 
        path[x] <- i
        if x <> e then 
            let next = 
                neighboors x 
                |> Array.filter (fun v-> item v <> '#' && not (path.ContainsKey v) ) 
                |> Array.exactlyOne

            inner next (i+1)

    inner s 0
    path

let pt1() = 
    let dist = DFS S E

    let rec cheat dist_origin u = 
        function 
        | 2 -> 
            if item u <> '#' && dist[u] + 2 < dist_origin then 
                [(dist_origin - dist[u] - 2)]
            else 
                []
        | k -> 
            [
                for v in neighboors u do
                    yield! cheat dist_origin v (k+1)
            ]
                
    let result = 
        dist.Keys 
        |> List.ofSeq 
        |> List.filter (fun x -> x <> E)
        |> List.collect (fun u -> cheat dist[u] u 0)
        |> List.filter(fun x -> x >= 100)
        |> List.length

    printfn "pt1 %i" result


let pt2 () = 
    let dist = DFS S E

    let result = 
        dist.Keys
        |> Array.ofSeq 
        |> Array.all_pairs
        |> Array.filter (
            fun (a,b) -> 
                let length = manhattanDistance a b
                let saving = abs ( dist[a]-dist[b] )
                length <= 20 && saving - length >= 100
        )
        |> Array.length

    printfn "pt2 %i" result
     
pt1() 
pt2()
