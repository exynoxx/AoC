module day18

open Utils
open System.Collections.Generic
open DataStructures
open System
open System.IO

let size = 70

let G = [|for _ in 0 .. size -> [|for _ in 0 .. size -> '.' |] |]
let walls = File.ReadAllLines("data/day18.txt") |> Array.take 1024

for xy in walls do
    let (x,y) = xy |> IntTupleOf ","
    G[y][x] <- '#'

(* for i in 0..size do 
    let s = String G[i]
    printfn "%s" s
 *)
let item (i,j) =
    if i >= 0 && i <= size && j >= 0 && j <= size then 
        G[i][j]
    else 
        '#'

let euclideanDistance (x1, y1) (x2, y2) =
    int (sqrt (float ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))))

let adj = [|(-1,0);(0,1);(1,0);(0,-1);|]
let A_star s e =

    let queue = MinHeap<int*(int*int), int>(fst)
    let dist = Dictionary<int*int, int>()

    dist[s] <- 0
    queue.Insert((0,s))

    while queue.Any() do
        let _, u = queue.RemoveMin()

        for i in 0..3 do
            let v = u++adj[i]

            if item v = '.' then
                let dist_v = dist.GetValueOrDefault(v, Int32.MaxValue)

                if dist[u]+1 < dist_v then
                    dist[v] <- dist[u]+1
                    let H = euclideanDistance (size,size) v
                    queue.Insert((dist[v]+H,v))

    dist
    
let pt1() = 

    let S = (0,0)
    let E = (size,size)
    let dist = A_star S E
    printfn "pt1 %i" dist[E]

pt1()
