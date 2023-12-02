namespace AoC

open System.Collections.Generic
open System.IO
open Util

module Day12 =
    let diff = [(-1,0);(1,0);(0,-1);(0,1)]
    let dijkstra reachable s =
        let q = PriorityQueue<int * int, int> ()
        let dist = Dictionary<int * int, int> ()
        let visited = HashSet<int*int> ()
        q.Enqueue (s,0)
        dist[s]<-0
               
        let rec bfs () =
            let mutable u = (-1,-1)
            let mutable distu = 0
            if q.TryDequeue(&u,&distu) then
                if visited.Contains u |> not then
                    visited.Add(u) |> ignore
                    let neighbors = diff
                                    |> Seq.map (fun d -> u++d)
                                    |> Seq.filter (reachable u)
                    for v in neighbors do
                        if dist[u]+1 < (dist.GetValOrDefault v 1000) then
                            dist[v] <- dist[u]+1
                            q.Enqueue (v,dist[v])
                    
                bfs ()
        bfs ()
        dist
    
    let Solution() =
        let start = int 'a'
        let destination = int 'z'
        let mutable s = (-1,-1)
        let mutable e = (-1,-1)
        
        let allset =
            File.ReadLines("../../../input12.txt")
            |> Seq.map (fun l -> Seq.toList l |> Seq.indexed)
            |> Seq.indexed
            |> Seq.map (fun (i, l) -> l |> Seq.map (fun (j, c) -> (i, j, c)))
            |> Seq.concat

        let height =
            allset
            |> Seq.map
                (fun (i, j, c) ->
                    match c with
                    | 'S' ->
                        s <- (i,j)
                        (i, j, start)
                    | 'E' ->
                        e <- (i,j)
                        (i, j, destination)
                    | _ -> (i, j, int c))
            |> Seq.map (fun (i, j, c) -> KeyValuePair((i, j), c))
            |> Dictionary
            
        let adj = height.Keys |> HashSet
        
        // PT1 
        let dist1 = dijkstra (fun u v -> adj.Contains v && height[v]-height[u] <= 1) s
        printfn $"pt1 {dist1[e]}"
        
        //PT2
        let dist2 = dijkstra (fun u v -> adj.Contains v && height[u]-height[v] <= 1) e
        let minA = height
                   |> Seq.filter (fun kv-> kv.Value = int 'a')
                   |> Seq.minBy (fun kv -> dist2.GetValOrDefault kv.Key 1000)
        
        printfn $"pt2 {dist2[minA.Key]}"
        


