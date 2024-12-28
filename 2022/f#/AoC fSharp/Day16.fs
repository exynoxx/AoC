namespace AoC

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Linq
open System.Text.RegularExpressions

module Day16 =
    
    let rx = Regex(@"Valve (\w{2}) has flow rate=(\d+); tunnel.? lead.? to valve.? (.+)$");

    let parseLine (s:string) =
        let captures = (rx.Match s).Groups |> Seq.map (fun x -> x.Value) |> List.ofSeq
        match captures with
        | [_;valve;flow;adj] -> (valve, Int32.Parse flow, adj.Split(", "))
        | _ -> raise (Exception "bad parse")
    
    let parse lines =
        let valves = lines |> Seq.map parseLine |> Array.ofSeq
        let index = valves |> Array.mapi (fun i (a,_,_) -> KeyValuePair(a,i)) |> Dictionary
        let adj = valves |> Array.map (fun (a,b,neighbors) -> Array.map (fun x -> index[x]) neighbors)
        let flows = valves |> Array.map (fun (a,b,c) -> b)
        let AA = index["AA"]
        adj,flows, AA
    
    let floydWarshall (adj:int[][]) =
        let n = adj.Length
        let dist = Array2D.init n n (fun _ _ -> 1000)
        for k = 0 to n - 1 do
            dist[k,k] <- 0
        
        for i=0 to n - 1 do
            for j in adj[i] do
                dist[i,j] <- 1
        for k=0 to n - 1  do
            for i=0 to n - 1  do
                for j=0 to n - 1  do
                    if dist[i,k] + dist[k,j] < dist[i,j] then
                        dist[i,j] <- dist[i,k] + dist[k,j]

        dist
    
    let (|SeqEmpty|SeqCons|) (l: 'a seq) = if Seq.isEmpty l then SeqEmpty else SeqCons l
    let mmax = function
        | SeqEmpty -> 0
        | SeqCons x -> Seq.max x
        
    let rec DFS1
        (adj:int[][])
        (flows:int[])
        (distances:int[,])
        (visited:Set<int>)
        u
        t
        flow
        (globalmax:int ref) : int =
        if t <= 0 then
            flow
        else
            let n = adj.Length
            let newvisit = visited.Add(u)
            let unvisited = seq{0 .. n-1 } |> Seq.filter (fun v -> not (newvisit.Contains v) && flows[v]>0)
            
            let remaining = unvisited |> Seq.sumBy (fun v -> t*flows[v])        
            if flow+remaining < !globalmax then
                0
            else
                let GetValue v =
                    let vt = t-distances[u,v]-1
                    let realizedFLow = vt * flows[v]
                    DFS1 adj flows distances newvisit v vt (flow+realizedFLow) globalmax
                    
                let max = unvisited |> Seq.map GetValue |> mmax
                if max > !globalmax then
                    printfn $"{max} max found, heuristic {flow+remaining}"
                    globalmax := max
                
                max
    
    let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, x::xs -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs
    
    let rec DFS2
        (adj:int[][])
        (flows:int[])
        (distances:int[,])
        (visited:Set<int>)
        (u1:int)
        (u2:int)
        (t1:int)
        (t2:int)
        flow
        (globalmax:int ref) : int =
        if t1 <= 0 && t2 <= 0 then
            if flow > !globalmax then
                printfn $"{flow} max found"
                globalmax := flow
            flow
        else
            let n = adj.Length
            let newvisited = visited.Add(u1).Add(u2)
            let unvisited = seq{0 .. n-1 }
                            |> Seq.filter (fun v -> not (newvisited.Contains v) && flows[v]>0)
                            |> List.ofSeq
            
            let remaining = unvisited |> Seq.sumBy (fun v -> t1*flows[v])  
            if false then
                0
            else
                
                let GetValue (v1:int) (v2:int) : int =
                    let vt1,realized1 = 
                        if t1>0 then 
                            let vt1 = t1-distances[u1,v1]-1
                            (vt1, vt1 * flows[v1])
                        else
                            (t1,0)
                    
                    let vt2, realized2 =
                        if t2>0 then
                            let vt2 = t2-distances[u2,v2]-1
                            (vt2, vt2 * flows[v2])
                        else
                            (t2,0)
                    DFS2 adj flows distances visited v1 v2 vt1 vt2 (flow+realized1+realized2) globalmax
                    
                comb 2 unvisited |> List.map (fun [a;b] -> GetValue a b) |> List.max 
                
    let pt1() =
        let raw = File.ReadLines("../../../input16.txt")
        let adj, flows, AA = parse raw
        let distances = floydWarshall adj
        let result = DFS1 adj flows distances Set.empty AA 30 0 (ref 0)
        printfn $"pt1 {result}"
        
    let pt2() =
        let raw = File.ReadLines("../../../input16.txt")
        let adj, flows, AA = parse raw
        let distances = floydWarshall adj
        let result = DFS2 adj flows distances Set.empty AA AA 26 26 0 (ref 0)
        printfn $"pt2 {result}"
                
            
    
    
                