module day18

open Utils
open System.Collections.Generic
open DataStructures
open System
open System.IO

let size = 70

let S = (0,0)
let E = (size,size)

let lines = File.ReadAllLines("data/day18.txt") |> Array.map (IntTupleOf ",")

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
let A_star item s e:Dictionary<int*int, int>*HashSet<int*int> =

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

    let path = if prev.ContainsKey e then backtrack prev e else HashSet<int*int>()
    dist,path

let G = [|for _ in 0 .. size -> [|for _ in 0 .. size -> '.' |] |]
let item (i,j) =
    if i >= 0 && i <= size && j >= 0 && j <= size then 
        G[i][j]
    else 
        '#'

for (x,y) in lines |> Array.take 1024 do
    G[y][x] <- '#' 

let pt1() = 
    let dist, path = A_star item S E
    printfn "pt1 %i" dist[E]

let pt2() = 

    let rec find_blocking (path:HashSet<int*int>) i = 
        let (x,y) = lines[i]
        G[y][x] <- '#'

        if path.Contains (y,x) then 
            //recompute
            let _,path = A_star item S E
            if path.Count = 0 then 
                (x,y)
            else 
                find_blocking path (i+1)
        else 
            find_blocking path (i+1)
    
    let init_path = snd (A_star item S E)
    let result = find_blocking init_path 1025

    printfn "pt2 %A" result

pt1()
pt2()
