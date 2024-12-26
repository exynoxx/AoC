module day18

open Utils
open System.Collections.Generic
open DataStructures
open System
open System.IO

let size = 70

let S = (0,0)
let E = (size,size)

let item_retriever (G: char array array) (i,j) =
    if i >= 0 && i <= size && j >= 0 && j <= size then 
        G[i][j]
    else 
        '#'

let euclideanDistance (x1, y1) (x2, y2) =
    int (sqrt (float ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))))

let backtrack (prev:Dictionary<int*int,int*int>) u = 
    let visited = HashSet<int*int>()
    let mutable cur = u
    while prev[cur] <> cur do 
        visited.Add cur |> ignore
        cur <- prev[cur]
    visited

let adj = [|(-1,0);(0,1);(1,0);(0,-1);|]
let A_star item s e:Dictionary<int*int, int>*Dictionary<int*int, int*int> =

    let queue = MinHeap<int*(int*int), int>(fst)
    let dist = Dictionary<int*int, int>()
    let prev = Dictionary<int*int, int*int>()

    dist[s] <- 0
    prev[s] <- s
    queue.Insert((0,s))
    let mutable search = true

    while queue.Any() && search do
        let _, u = queue.RemoveMin()

        if u = e then 
            search <- false

        for i in 0..3 do
            let v = u++adj[i]

            if item v = '.' then
                let dist_v = dist.GetValueOrDefault(v, Int32.MaxValue)

                if dist[u]+1 < dist_v then
                    dist[v] <- dist[u]+1
                    prev[v] <- u
                    let H = euclideanDistance (size,size) v
                    queue.Insert((dist[v]+H,v))

    dist, prev
    
let fall (G: char array array) line = 
    let (x,y) = line |> IntTupleOf ","
    G[y][x] <- '#'

let parse s = s |> IntTupleOf ","

let pt1() = 

    let G = [|for _ in 0 .. size -> [|for _ in 0 .. size -> '.' |] |]
    let item = item_retriever G
    let lines = File.ReadAllLines("data/day18.txt") |> Array.take 1024

    for line in lines do
        fall G line
    
    let dist, _ = A_star item S E
    printfn "pt1 %i" dist[E]

let pt2() = 
    let G = [|for _ in 0 .. size -> [|for _ in 0 .. size -> '.' |] |]
    let item = item_retriever G
    let input = File.ReadAllLines("data/day18.txt") |> Array.map parse

    for (x,y) in input |> Array.take 1024 do
        G[y][x] <- '#'

    let mutable dist, prev = A_star item S E
    let mutable path = backtrack prev E

    let mutable i = 1025
    let mutable search = true
    while search || i < input.Length-1 do 
        let (x,y) = input[i]
        G[y][x] <- '#'

        if path.Contains (y,x) then 
            let d,p = A_star item S E
            dist <- d
            prev <- p

            if not (dist.ContainsKey E) then 
                printfn "pt2 %i,%i" x y
                Environment.Exit(0)
            else
                path <- backtrack prev E

        i <- i + 1



pt1()
pt2()
