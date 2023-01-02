namespace AoC

open System
open System.Collections.Generic
open System.IO
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
            let unvisited = seq{0 .. n-1 } |> Seq.filter (fun v -> not (newvisit.Contains v) && flows[v]>0) |> Array.ofSeq
            
            let remaining = unvisited |> Array.sumBy (fun v -> t*flows[v])        
            if flow+remaining < !globalmax then
                0
            else
                let inline GetValue v =
                    let vt = t-distances[u,v]-1
                    let realizedFLow = vt * flows[v]
                    DFS1 adj flows distances newvisit v vt (flow+realizedFLow) globalmax
                    
                let max = unvisited |> Array.map GetValue |> Array.max
                if max > !globalmax then
                    printfn $"{max} max found, heuristic {flow+remaining}"
                    globalmax := max
                
                max
          
    let rec DFS2
        (adj:int[][])
        (flows:int[])
        (distances:int[,])
        (visited:Set<int>)
        (u1:int)
        (u2:int)
        (t:int)
        flow
        (globalmax:int ref) : int =
        if t <= 0 then
            flow
        else
            let n = adj.Length
            let newvisited = visited.Add(u1).Add(u2)
            let unvisited = seq{0 .. n-1 } |> Seq.filter (fun v -> not (newvisited.Contains v) && flows[v]>0)
            
            let remaining = unvisited |> Seq.sumBy (fun v -> (t/2)*flows[v])  
            if flow+remaining < !globalmax then
                0
            else
                let GetValue (visited:Set<int>) (u:int) (v:int) =
                    let vt = t-distances[u,v]-1
                    let realizedFLow = vt * flows[v]
                    DFS1 adj flows distances visited v vt (flow+realizedFLow) globalmax
                    
                let neighborValues = unvisited |> Seq.map (GetValue newvisited u1)
                let v1,max1 = Seq.zip unvisited neighborValues |> Seq.maxBy snd
                let max2 = unvisited
                              |> Seq.filter (fun x -> x <> v1)
                              |> Seq.map (GetValue (newvisited.Add v1) u2)
                              |> Seq.max
                let max = max1+max2
                if max2 > !globalmax then
                    printfn $"{max} max found, heuristic {flow+remaining}"
                    globalmax := max
                
                max1  
                
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
        let result = DFS2 adj flows distances Set.empty AA AA 26 0 (ref 0)
        printfn $"pt2 {result}"
                
            
    
    
                