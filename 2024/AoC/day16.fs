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

//        [|NORTH;EAST;SOUTH;WEST;|]
let adj = [|(-1,0);(0,1);(1,0);(0,-1);|]
let dijkstra s e =

    let queue = MinHeap<int*State, int>(fst)
    let dist = Dictionary<int*int, int>()
    let prev = Dictionary<int*int, (int*int) list>()

    dist[s] <- 0
    prev[s] <- [s]
    queue.Insert((0,{Orientation=1; Pos = s}))

    while queue.Any() do
        let cost, {Orientation=ori;Pos=u} = queue.RemoveMin()

        if u <> e then

            for i in 0..3 do
                let v = u++adj[i]

                let price = 
                    match (ori-i+4)%4 with
                    | 0 -> 1
                    | 2 -> 2001
                    | 1 | 3 -> 1001

                if item v <> '#' && price <> 2001 then

                    if v = (7,15) then
                         ()

                    let dist_v = dist.GetValueOrDefault(v, Int32.MaxValue)

                    if dist[u]+price-1000 = dist_v then
                        prev[v] <- u::prev[v]

                    if dist[u]+price < dist_v then
                        dist[v] <- dist[u]+price
                        prev[v] <- [u]
                        queue.Insert((dist[v],{Orientation=i;Pos=v}))

    dist, prev
    
let S = get_element 'S'
let E = get_element 'E'

let dist, prev = dijkstra S E

let pt1() = 

    printfn "pt1 %i" dist[E]

let pt2 () =
    let positions = HashSet<int*int>()

    let rec Backwards u = 
        positions.Add u |> ignore
        match prev[u] with
        | [] -> ()
        | [x] when x = S -> ()
        | [x] -> 
            Backwards x
        | xs -> 
            for x in xs do 
                Backwards x

    Backwards E
    printfn "pt2 %i" positions.Count

pt1()
pt2()