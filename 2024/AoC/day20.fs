module day20

open Utils
open System.Collections.Generic
open DataStructures
open System

let G = ParseGrid("data/day20.txt")

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

let dijkstra (source: int*int) =

    let queue = MinHeap<int*(int*int), int>(fst)
    let dist = Dictionary<int*int, int>()
    let prev = Dictionary<int*int,int*int>()

    queue.Insert((0, source))
    dist[source] <- 0
    prev[source] <- source

    while queue.Any() do

        let d, u = queue.RemoveMin()

        for v in neighboors u do 
            if item v <> '#' && dist[u]+1 < dist.GetValueOrDefault(v,Int32.MaxValue) then 
                dist[v] <- dist[u]+1 
                prev[v] <- u 
                queue.Insert ((dist[v],v))
        
    dist, prev

let pt1() = 
    let dist, path = dijkstra E

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
        path.Keys 
        |> List.ofSeq 
        |> List.filter (fun x -> x <> E)
        |> List.collect (fun u -> cheat dist[u] u 0)
        |> List.filter(fun x -> x >= 100)
        |> List.length

    printfn "pt1 %i" result


let pt2 () = 
    let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    let dist, prev = dijkstra E

    (* let rec get_path u acc = 
        match prev[u] with
        | x when x = u -> acc
        | x -> get_path x (x::acc) *)

    let se1 = dist[S]
    let se2 = manhattanDistance S E

    //let path = get_path S []

    let cheats = 
        prev.Keys
        |> Array.ofSeq 
        |> Array.all_pairs
        |> Array.filter (fun (a,b) -> manhattanDistance a b <= 20)
        |> Array.collect (fun (a,b) -> [| (a,b); (b,a) |])
        |> Array.where (fun (a,b) -> dist[b]+20 < dist[a])
        |> Array.map (fun (a,b) -> dist[a]-dist[b]-manhattanDistance a b)
        |> Array.where (fun saves -> saves >= 100)

    printfn "pt2 %i" cheats.Length
     
//pt1()
pt2() //1016066