module day11

open System.IO
open System.Collections.Generic
open System


let dijkstra n m (emptyRows:HashSet<int>) (emptyCols:HashSet<int>) s expansionFactor =

    let Q = PriorityQueue<int*int, int64>()
    let visited = HashSet<int*int>()
    let dist = new Dictionary<int*int,int64>()

    visited.Add s |> ignore
    dist[s] <- 0
    Q.Enqueue (s,0)

    while Q.Count > 0 do
        let u = Q.Dequeue()

        visited.Add(u) |> ignore
        let adj = [(0,1);(0,-1);(1,0);(-1,0)]

        for diff in adj do
            let v = (fst diff + fst u, snd diff + snd u)
            let (v1,v2) = v
            if v1 >= 0 && v1 < n && v2 >= 0 && v2 < m && not (visited.Contains v) then
                let factor = int64 <| match diff with
                                        | (_,0) when emptyRows.Contains (fst u) -> expansionFactor
                                        | (0,_) when emptyCols.Contains (snd u) -> expansionFactor
                                        | _ -> 1

                if dist[u] + factor < dist.GetValueOrDefault(v,Int64.MaxValue) then
                    dist[v] <- dist[u] + factor
                    Q.Enqueue (v,dist[v])
    dist

let GetemptyRows (graph: char[][]) = seq { 0 .. graph.Length - 1} |> Seq.where (fun i -> graph[i] |> Array.forall (fun c -> c = '.')) |> HashSet
let GetemptyCols (graph: char[][]) = seq { 0 .. graph[0].Length - 1} |> Seq.filter (fun j -> graph |> Array.forall (fun row -> row[j] = '.')) |> HashSet
   
let Getgalaxies (graph: char[][]) =
    seq {
        for i in 0 .. graph.Length - 1 do
            for j in 0 .. graph[0].Length - 1 do
                if graph[i][j] = '#' then
                    yield (i,j)
    } |> HashSet  


let grid = File.ReadAllLines("day11.txt") |> Seq.map _.ToCharArray() |> Array.ofSeq
let n = grid.Length
let m = grid[0].Length
let emptyRows = GetemptyRows(grid)
let emptyCols = GetemptyCols(grid)
let galaxies = Getgalaxies grid
let runDijkstra = dijkstra n m emptyRows emptyCols

let GetSum expansionFactor =
    let covered = new HashSet<(int*int)*(int*int)>()
    let mutable sum = 0L

    for galaxy in galaxies do

        let dist = runDijkstra galaxy expansionFactor
        for otherGalaxy in galaxies do
            
            let pair = (galaxy, otherGalaxy)
            if galaxy <> otherGalaxy && not (covered.Contains pair) then

                printfn $"galaxy {pair} {dist[otherGalaxy]}"
                sum <- sum + dist[otherGalaxy]
                covered.Add (galaxy, otherGalaxy) |> ignore
                covered.Add (otherGalaxy, galaxy) |> ignore
    sum

let pt1() = printfn $"sum {GetSum 2}"
let pt2() = printfn $"sum {GetSum 1000000}"

pt2()

