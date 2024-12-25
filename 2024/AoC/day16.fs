module day16

open Utils
open System.Collections.Generic
open DataStructures
open System

type State = {Orientation:int;Pos:int*int}

let G = ParseGrid("data/day16.txt")

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


//                            [|NORTH;EAST;SOUTH;WEST;|]
let adj = [|(-1,0);(0,1);(1,0);(0,-1);|]
(* let neighboors u = 
    adj 
    |> Array.map (fun dv -> dv++u) 
    |> Array.filter (fun v -> item v <> '#')

 *)
let dijkstra s e =

    let queue = MinHeap<int*State, int>(fst)
    let dist = Dictionary<(int*int), int>()

    dist[s] <- 0
    queue.Insert((0,{Orientation=1; Pos = s}))

    while queue.Any() do
        let {Orientation=ori;Pos=u} = snd (queue.RemoveMin())

        if u <> e then

            for i in 0..3 do
                let v = u++adj[i]

                if item v <> '#' then

                    let cost = match (ori+4-i)%4 with
                               | 0 -> 1
                               | 2 -> 2001
                               | _ -> 1001

                    if dist[u]+cost < dist.GetValueOrDefault(v, Int32.MaxValue) then
                        dist[v] <- dist[u]+cost
                        queue.Insert((dist[v],{Orientation=i;Pos=v}))

    dist[e]
    

let pt1() = 

    let S = get_element 'S'
    let E = get_element 'E'
    let dist_e = dijkstra S E

    printfn "pt1 %i" dist_e

pt1()
