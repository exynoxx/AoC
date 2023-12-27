module day23

open System.IO
open System.Collections.Generic
open utils 

let pt1() = 
    let grid = 
        File.ReadAllLines("day23.txt") 
        |> Array.map _.ToCharArray()
    
    let n = grid.Length
    let m = grid[0].Length

    grid[0][1] <- '#'
    grid[n-1][m-2] <- '#'
    let s = (1,1)
    let t = (n-2, m-2)

    let valid (diffx,diffy) (x,y) = 
        let (a,b) = (diffx+x,diffy+y)
        match grid[x][y] with
        | '^' -> (diffx,diffy) = (-1,0)
        | 'v' -> (diffx,diffy) = (1,0)
        | '>' -> (diffx,diffy) = (0,1)
        | '<' -> (diffx,diffy) = (0,-1)
        | '.' -> grid[a][b] <> '#'


    let visited = new HashSet<int*int>()
    let mutable maxx = 0
    let rec dfs u =
        if u = t then 
            0
        else
            visited.Add u |> ignore

            let best =
                [(1,0); (-1,0); (0,-1); (0,1)]
                |> List.filter (fun diff -> valid diff u)
                |> List.map (fun diff -> diff ++ u)
                |> List.filter (fun v -> not (visited.Contains v))
                |> List.map (fun v -> 1 + dfs v)
                |> listmax 0
                    
            visited.Remove u |> ignore
            best

    let answer = 2 + dfs s
    printfn $"{answer}"

let pt2 () = 
    let grid = 
        File.ReadAllLines("day23.txt") 
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

    let visited = new HashSet<int*int>()
    let mutable maxx = 0
    let rec dfs u l =
        if u = t then 
            if l+2 > maxx then 
                maxx <- l+2
                printfn $"max {maxx}"
        else
            visited.Add u |> ignore

            for v in adj[u].Keys do
                if not(visited.Contains v) then
                    dfs v (l+adj[u][v])
                    
            visited.Remove u |> ignore

    let answer = dfs s 0
    printfn $"{answer}"

pt1()
pt2()
