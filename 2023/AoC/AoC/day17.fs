module day17

open System.Collections.Generic
open System.IO
open System 
open utils

let dijkstra (grid:int array array) n m s =
    let Q = PriorityQueue<int*int, int>()
    let visited = HashSet<int*int>()
    let dist = new Dictionary<int*int,int>()
    let prev = new Dictionary<int*int,(int*int)*int>()

    dist[s] <- 0
    prev[s] <- ((0,0),0) //apparently it came from west and went east to goto s
    Q.Enqueue (s,0)

    while Q.Count > 0 do
        let u = Q.Dequeue()

        visited.Add(u)

        for diff in [(0,1);(0,-1);(1,0);(-1,0)] do
            let v = diff ++ u
            let (v1,v2) = v
            if v1 >= 0 && v1 < n && v2 >= 0 && v2 < m then

                if dist[u] + grid[v1][v2] < dist.GetValueOrDefault(v,Int32.MaxValue) && not (visited.Contains v) then

                    
                    let (prevdirection,d) = prev[u]

                    let steps = if prevdirection = diff then d+1 else 1
                    if steps <= 3 then
                        dist[v] <- dist[u] + grid[v1][v2]
                        prev[v] <- (diff,steps)
                        Q.Enqueue (v,dist[v])



                    
    dist

let pt1 () = 
    let grid = File.ReadAllLines("day17.txt") |> Seq.map (fun l -> l.ToCharArray() |> Array.map (fun c -> int c - int '0' )) |> Array.ofSeq 

    let n = grid.Length
    let m = grid[0].Length

    let dist = dijkstra grid n m (0,0)
    let answer = dist[(n-1,m-1)]
    printfn $"{answer}"

pt1()