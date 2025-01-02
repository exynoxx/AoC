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

let dijkstra s e =

    let queue = MinHeap<int*(int*int), int>(fst)
    let dist = Dictionary<int*int, int>()
    let prev = Dictionary<int*int, (int*int) list>()

    let arrow = Dictionary<int*int, int>()
    let next = Dictionary<int*int, int*int>()

    dist[s] <- 0
    arrow[s] <- 1
    prev[s] <- [s]
    queue.Insert((dist[s],s))

    while queue.Any() do
        let cost, u = queue.RemoveMin()

        if u <> e then

            for i in 0..3 do
                let v = u++adj[i]

                let price = (turn_price arrow[u] i)
                if item v <> '#' then
                    let dist_v = dist.GetValueOrDefault(v, Int32.MaxValue)

                    if dist[u] + price + 1 < dist_v then 
                        dist[v] <- dist[u] + price + 1
                        arrow[v] <- i
                        prev[v] <- [u]
                        next[u] <- v

                        queue.Insert((dist[v],v))
                    
                    elif arrow[u] = arrow[v] && dist[u] + 2 = dist[next[v]] then 
                        prev[v] <- u::prev[v]
                    elif arrow[u] <> arrow[v] && dist[u] + 2 + 1000 = dist[next[v]] then 
                        prev[v] <- u::prev[v]

    dist, prev
    
let S = get_element 'S'
let E = get_element 'E'

let dist, prev = dijkstra S E

let pt1() = 
    printfn "pt1 %i" dist[E]

let pt2 () =
    let positions = HashSet<int*int>()

    let shortest = dist[E] % 1000

    let rec Backwards u i = 

        if shortest - (dist[u]%1000) <> i then 
            ()

        positions.Add u |> ignore
        match prev[u] with
        | [] -> ()
        | _ when u = S -> 
            if i = shortest then 
                ()
            else
                ()
        | [x] -> Backwards x (i+1)

        | xs ->     
            for x in xs do 
                Backwards x (i+1)

    Backwards E 0

    for (i,j) in positions do 
        G[i][j] <- 'O'

    for row in G do
        printf "     "
        row |> Array.iter (printf "%c")
        printfn ""

    printfn "pt2 %i" positions.Count
    //552 too high

pt1()
pt2()