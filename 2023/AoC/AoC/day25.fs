module day25

open System.Collections.Generic
open System.IO
open utils

open System

let parse(s:string) = 
    let [|node;adj|] = s.Split(": ")
    let adjSet = adj.Split(" ") |> HashSet
    (node,adjSet)

let getGraph () = 

    let protoGraph = File.ReadAllLines("day25.txt") |> Array.map parse

    let G = new Dictionary<string,HashSet<string>>()

    for (u,adj) in protoGraph do
        G[u] <- new HashSet<_>()
        for v in adj do 
            if not (G.ContainsKey v) then 
                G[v] <- new HashSet<_>()

    for (u,adj) in protoGraph do
        G[u].UnionWith(adj)
        for v in adj do
            G[v].Add u |> ignore

    G
let pt1() = 
    let rnd = System.Random ()
    
    let shuffle seq =
        seq |> Seq.sortBy (fun _ -> rnd.Next(100))
    
    let G = getGraph()

    //let E = [for k in G.Keys do for v in G[k] do yield (k,v)]
    
    let heat = new Dictionary<string*string,int>()

    let Find s t = 
        let path = new HashSet<_>()
        let rec dfsInner u = 
            if u <> t then 
                path.Add u
                for v in G[u] |> shuffle do
                    if not(path.Contains v) then 
                        heat[(u,v)] <- heat.GetValueOrDefault ((u,v), 0) + 1
                        dfsInner v
                    
        dfsInner s

    for i in G.Keys do
        for j in G.Keys do 
            if i <> j then 
                Find i j


    let l = heat |> Seq.sortByDescending _.Value |> List.ofSeq
    ()

pt1()

(*let n = E.Length
    let exlucdedSets = 
        seq { 
            for i in 0 .. n - 3 do
                for j in i + 1 .. n - 2 do
                    for k in j + 1 .. n - 1 do
                        yield seq{ E[i]; E.[j]; E[k]} |> HashSet }*)

    (*let Find s t = 
        let path = new HashSet<_>()
        let rec dfsInner u = 
            if u = t then 
                ()
            else
                path.Add u

                match G.TryGetValue u with
                | false, _ -> ()
                | true, adj -> 
                    for v in adj do
                        if not(path.Contains v) then 
                            dfsInner v
        dfsInner s
        path 

    let mutable maxx = 0
    let mutable i = 0
    for excluded in exlucdedSets do 
        let (a,b) = excluded |> Seq.find(fun _ -> true)
        let reach = Reach a excluded
        if not (reach.Contains b) && reach.Count > 1 then 
            let v = reach.Count * (vertices.Count-reach.Count)
            maxx <- max maxx v
            i <- i + 1
            printfn $"{maxx} {v} {i}"*)
(*let maxFlow () = 
       
        let flow = new Dictionary<string,Dictionary<string,int>>()

        let Dfs s t = 
            let path = new HashSet<_>()
            let rec dfsInner u = 
                if u = t then 
                    0
                else
                    path.Add u

                    let mutable pathMinValue = 2
                    for v in G.GetValueOrDefault(u, new HashSet<_>()) do
                        if flow[u][v] < 1 && not(path.Contains v) then 
                            let result = dfsInner v
                            match result with
                            | 0 -> flow[u][v] <- 1
                            | _ -> ()

                            pathMinValue <- min pathMinValue result

                    path.Remove u

                    pathMinValue
            dfsInner s


        let s = G.Keys |> Seq.find(fun _ -> true)
        for t in Seq.filter(fun v -> v<>s) G.Keys do
            
            flow.Clear()

            for u in G.Keys do 
                flow[u] <- new Dictionary<string,int>()
                for v in G[u] do
                    flow[u][v] <- 0
            
            let mutable sum = 0
            let mutable result = 1
            while result = 1 do 
                sum <- sum + 1
                result <- Dfs s t
            printfn $"{s} {t} {sum}"


    maxFlow()*)

(*let Reach s (exluded:HashSet<_>) = 
        let path = new HashSet<_>()
        let rec dfsInner u = 
            path.Add u

            for v in G.GetValueOrDefault(u,new HashSet<_>()) do
                if not(path.Contains v) && not (exluded.Contains (u,v)) then 
                    dfsInner v
        dfsInner s
        path 

    for e1 in E do
        for e2 in E do 
            for e3 in E do
                if e1 <> e2 && e2 <> e3 && e3 <> e1 then 
                    let exluded = seq {e1;e2;e3} |> HashSet
                    let (a,b) = e1
                    let reach = Reach a exluded
                    if not (reach.Contains b) then 
                        let reachb = Reach b exluded
                        printfn $"{reach.Count * reachb.Count}"*)





    (*let karger () = 
        let r = new Random()

        let countV () =
            let hs = seq {for (a,b) in E do yield a; yield b;} |> HashSet 
            hs.Count

        while countV() > 2 do
            E <- List.removeAt (r.Next(E.Length - 1)) E
        
        ()

    karger () *)

(*let maxFlow () = 
       
        let flow = new Dictionary<string,Dictionary<string,int>>()

        let Dfs s t () = 
            let path = new HashSet<_>()
            let mutable bottleNecks = []
            let rec dfsInner u maxValue = 
                if u = t then 
                    maxValue
                else
                    path.Add u

                    let mutable pathMinValue = 2
                    for v in G[u].Keys do
                        if flow[u][v] < 1 && not(path.Contains v) then 
                            let result = dfsInner v t (min maxValue 1)
                            pathMinValue <- min pathMinValue result

                    path.Remove u

                    match pathMinValue with
                    | 0 -> flow[u][v] <- 1
                    | _ -> ()

                    pathMinValue
            dfsInner s 0


        let s = G.Keys |> Seq.find(fun _ -> true)
        for t in Seq.filter(fun v -> v<>s) G.Keys do
            
            flow.Clear()

            for u in G do 
                flow[u] <- new Dictionary<string,int>()
                for v in G[u] do
                    flow[u][v] <- 0
            
            let result = 1
            while result = 1 do 
                result <- Dfs s t

*)

(*let maxFlow () = 


    let flow = new Dictionary<string,int>()

    for k in G

    let r = new Random()
    let s = G.Keys |> List.ofSeq |> List.item (r.Next(G.Count - 1))
    let visited = new HashSet<_>()
    let dfsFill u t d = 
        if u = t then 
            flowDict[s][t] <- min flowDict[s][t] d
        else
            visited.Add u

            for v in G[u] do
                if not (visited.Contains v) && flow u v = 0 then 
                    dfsFill v t (d + 1)
*)
(*let flow u v = 
        match flowDict[u].TryGetValue v with
        | (true, x) -> x
        | (false,_) -> 0*)