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

let turn_price src tgt = 
    match (src-tgt+4)%4 with
    | 0 -> 0
    | 2 -> 2000
    | _ -> 1000

let dijkstra s e =

    let queue = MinHeap<int*State, int>(fst)
    let dist = Dictionary<int*int, int>()
    let prev = Dictionary<int*int, (int*int) list>()

    dist[s] <- 0
    prev[s] <- [s]
    queue.Insert((0,{Orientation=1; Pos = s}))

    while queue.Any() do
        let cost, {Orientation=ori_u;Pos=u} = queue.RemoveMin()

        if u <> e then

            for i in 0..3 do
                let v = u++adj[i]

                let price = (turn_price ori_u i) + 1
                if item v <> '#' then
                    let dist_v = dist.GetValueOrDefault(v, Int32.MaxValue)

                    if dist[u]+price < dist_v then 
                        dist[v] <- dist[u]+price
                        prev[v] <- [u]

                        queue.Insert((dist[v],{Orientation=i;Pos=v}))

                    elif v <> e && dist[u] + 1 - 1000 = dist_v then 
                        prev[v] <- u::prev[v]

    dist, prev
    
let S = get_element 'S'
let E = get_element 'E'

let dist, prev = dijkstra S E

let pt1() = 

    printfn "pt1 %i" dist[E]

let pt2 () =
    let positions = HashSet<int*int>()
    let falsee = HashSet<int*int>()

    let rec Backwards acc i u = 
        match prev[u] with
        | [] -> ()
        | _ when u = S -> 
            if abs(i - (dist[E] % 1000)) <= 0 then 
                for x in acc do positions.Add x

        | [x] -> Backwards (u::acc) (i+1) x

        | xs -> 
            //1000 steps == 1 turn
            (*if acc.Length > 0 then 
                let last_intersection = List.last acc
                let len = acc.Length
                let correct_len =  (dist[last_intersection] - dist[u]) % 1000

                if abs(len - correct_len) <= 1 then 
                    (u::acc) |> List.map positions.Add |> ignore
                else
                    acc |> List.map falsee.Add |> ignore

                for x in xs do 
                    Backwards [u] 0 x
            else    
                for x in xs do 
                   Backwards (u::acc) 0 x *)
            for x in xs do 
                Backwards (u::acc) (i+1) x

    Backwards [E] 0 E

    for (i,j) in positions do 
        G[i][j] <- 'O'

    for (i,j) in falsee do 
        G[i][j] <- 'X'

    for row in G do
        row |> Array.iter (printf "%c")
        printfn ""

    printfn "pt2 %i" positions.Count
    //552 too high

pt1()
pt2()