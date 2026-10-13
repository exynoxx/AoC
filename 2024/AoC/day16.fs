module day16

open Utils
open System.Collections.Generic
open DataStructures
open System
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

let invert direction  = (direction + 2) % 4

let dijkstra s e =

    let queue = MinHeap<int*(int*int), int>(fst)
    let dist = Dictionary<int*int, int>()
    let prev = Dictionary<int*int, (int*int) list>()
    let next = Dictionary<int*int, (int*int)>()
    let arrow = Dictionary<int*int, int>()

    dist[s] <- 0
    arrow[s] <- 1
    prev[s] <- [s]
    queue.Insert((dist[s],s))

    while queue.Any() do
        let _, u = queue.RemoveMin()

        if u <> e then

            for j in 0..3 do
                let v = u++adj[j]

                let price = (turn_price arrow[u] j)
                if item v <> '#' && price <> 2000 then

                    let dist_v = dist.GetValueOrDefault(v, Int32.MaxValue)
                    if dist[u] + price + 1 < dist_v then 
                        dist[v] <- dist[u] + price + 1
                        prev[v] <- [u]
                        arrow[v] <- j

                        (*  if not (arrow.ContainsKey v) then 
                            arrow[v] <- j
                            next[u] <- v *)

                        queue.Insert((dist[v],v))

                    //Catch all, filter later
                    elif dist[u] + 1 - 1000 = dist_v || dist[u] + 1 = dist_v (* && next.ContainsKey v && arrow[u] = arrow[next[v]]  *) then 
                        prev[v] <- u::prev[v]
(* 
                    elif dist[u] + 1 + 1000 = dist_v && next.ContainsKey v && arrow[u] <> arrow[next[v]]  then 
                        prev[v] <- u::prev[v] *)

    dist, arrow, prev
    
let S = get_element 'S'
let E = get_element 'E'

let dist, arrow, prev = dijkstra S E

let pt1() = 
    printfn "pt1 %i" dist[E]

let pt2 () =
    let positions = HashSet<int*int>()

    let shortest = dist[E] % 1000

    let rec Backwards u dir = 
        positions.Add u |> ignore
        match prev[u] with
        | [] -> ()
        | _ when u = S -> ()
        | [x] -> Backwards x (i+1)
        | xs ->     
            for x in xs do 
                Backwards x (i+1)

    Backwards E (invert arrow[E])

    for (i,j) in positions do 
        G[i][j] <- 'O'

    for row in G do
        //printf "     "  
        row |> Array.iter (printf "%c")
        printfn ""

    printfn "pt2 %i" positions.Count
    //552 too high

pt1()
pt2()