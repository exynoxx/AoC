module day23

open System.IO
open System.Collections.Generic
open utils 
open System.Collections.Immutable
open System

type HashSetChain<'T> = {prev: HashSetChain<'T> option; set:HashSet<'T>; mutable size:int}

type HashSetChain<'T> with
    member this.Contains(x: 'T) =
        let rec ContainsInner path =
            if path.set.Contains x then 
                true
            else 
                match path.prev with
                | Some prev -> ContainsInner prev
                | None -> false

        ContainsInner this
    member this.Count() =
        
        let rec countInner set =
            match set.prev with
            | Some prev -> set.set.Count + countInner prev
            | None -> set.set.Count

        countInner this
    member this.Add x = 
        if this.set.Add x then
            this.size <- this.size + 1

    member this.Branch () = {prev = Some this; set = new HashSet<'T>();size=this.size}

    static member Create () = {prev = None; set = new HashSet<'T>();size=0}

let rec pathContains (path:HashSetChain<int*int>) x = 
    if path.set.Contains x then 
        true
    else 
        match path.prev with
        | Some prev -> pathContains prev x
        | None -> false



let pt1() = 
    let grid = 
        File.ReadAllLines("/home/nicholas/Documents/git/AoC/2023/AoC/AoC/day23.txt") 
        |> Array.map _.ToCharArray()
    
    let n = grid.Length
    let m = grid[0].Length

    let valid (diffx,diffy) (x,y) = 
        let (a,b) = (diffx+x,diffy+y)
        if a < 0 || a >= n || b < 0 && b >= m then
            false
        else
            match grid[x][y] with
            | '^' -> (diffx,diffy) = (-1,0)
            | 'v' -> (diffx,diffy) = (1,0)
            | '>' -> (diffx,diffy) = (0,1)
            | '<' -> (diffx,diffy) = (0,-1)
            | '.' -> grid[a][b] <> '#'

    let mutable candidates = []
    let rec bfs (queue:((int*int) * HashSetChain<int*int>) list) =
        match queue with
        | [] -> -1
        | (u,path)::rest ->
            if u = (n-1, m-2) then 
                candidates <- (path.Count())::candidates

            path.set.Add u |> ignore

            let validNeighbors =
                [(1,0); (-1,0); (0,-1); (0,1)]
                |> List.filter (fun diff -> valid diff u)
                |> List.map (fun diff -> diff ++ u)
                |> List.filter (fun v -> not (path.Contains v))

            match validNeighbors with
            | [] -> bfs rest
            | [v] -> bfs ((v,path)::rest)
            | vs -> 
                let elements = vs |> List.map (fun v -> (v,path.Branch()))
                bfs (rest@elements)


    let visited = HashSetChain<int*int> .Create()
    let _ = bfs [((0,1),visited)]
    let max = candidates |> List.max

    printfn $"{max}"



//find loops reduce

let pt2 () = 
    let grid = 
        File.ReadAllLines("/home/nicholas/Documents/git/AoC/2023/AoC/AoC/day23.txt") 
        |> Array.map _.ToCharArray()
    
    let n = grid.Length
    let m = grid[0].Length

    grid[0][1] <- '#'
    grid[n-1][m-2] <- '#'
    let s = (1,1)
    let t = (n-2, m-2)

    let adj = new Dictionary<int*int, Dictionary<int*int,int>>()
    for i in 0 .. n - 1 do
        for j in 0 .. m - 1 do 
            if(grid[i][j] <> '#') then
                let weights = new Dictionary<_,_>()
                for diff in [(1,0); (-1,0); (0,-1); (0,1)] do
                    let (a,b) = (i,j) ++ diff
                    if grid[a][b] <> '#' then
                        weights[(a,b)] <- 1 
                adj[(i,j)] <- weights
    
    let rec Prune () = 
        if adj.Values |> Seq.exists (fun (v:Dictionary<_,_>) -> v.Count = 2)  then
            for k in adj.Keys do
                if adj[k].Count = 2 then
                    let [a;b] = adj[k].Keys |> List.ofSeq
                    let w = adj[k][a] + adj[k][b]
                    adj.Remove k |> ignore
                    adj[a].Remove k |> ignore
                    adj[b].Remove k |> ignore
                    adj[a][b] <- w
                    adj[b][a] <- w
            Prune ()
            
    Prune ()

    let all = adj.Keys |> HashSet
    for i in 0 .. n - 1 do
        for j in 0 .. m - 1 do 
            if all.Contains ((i,j)) then
                printf "%c " '¤'
            else
                printf "  "
        printfn $"{i}"
        
    let bellmanFord () =
        let dist = Dictionary<int*int,int>()
        for k in adj.Keys do
            dist[k] <- Int32.MaxValue

        dist[s] <- 0

        for _ in adj.Keys do
            for u in adj.Keys do
                for v in adj[u].Keys do
                    let w = adj[u][v]
                    if dist[u] + w < dist[v] then
                        dist[v] <- dist[u] + w

        dist[t]
    
    let answer = bellmanFord()
    printfn $"{answer}"

pt2()

(* let longestPath () =
        let cache = new Dictionary<int*int,int*ImmutableHashSet<int*int>>()
        
        let mutable debug = 0
        let rec dfs (path:ImmutableHashSet<int*int>) u =
            if u = t then 
                0
            else
                let best = 
                    adj[u].Keys 
                    |> Seq.filter (fun v -> not(path.Contains v))
                    |> Seq.map (fun v -> adj[u][v] + dfs (path.Add u) v)
                    |> seqmax 0

                if best > debug then
                    debug<-best
                    printfn $"{debug}"

                best

        2 + dfs (ImmutableHashSet.Create()) s *)

(* let mutable maxD = 0
                for v in adj[u].Keys do
                    if not(path.Contains v) then
                        let w = adj[u][v]
                        dist[v] <- max (dist.GetValueOrDefault(v,0)) (dist[u]+w)
                        maxD <- max maxD (dfs (d+dist[v]) v)
                maxD *)

 (* let visited = new HashSet<int*int>()
    let dist = new Dictionary<int*int,int>()
    dist[s]<-0

    let queue = new PriorityQueue<int*int,int>()
    queue.Enqueue (s,0)

    let rec dijkstra () =
        match queue.TryDequeue() with
        | false,_,_ ->  2 - dist[t]
        | true,u,_ ->
            if u = t then
                let a = 2 - dist[t]
                a
            else 
                if not (visited.Contains u) then
                    visited.Add u |> ignore
                    for v in adj[u].Keys do
                        if dist[u] + adj[u][v] < dist.GetValueOrDefault(v, Int32.MaxValue) then
                            dist[v] <- dist[u] + adj[u][v]
                            queue.Enqueue (v, - dist[v])
                
                dijkstra() *)