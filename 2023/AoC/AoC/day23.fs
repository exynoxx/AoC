module day23

open System.IO
open System.Collections.Generic
open utils 

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
            | '#' -> false
            | '.' -> 
                match (diffx,diffy) with
                | _ when grid[a][b] = '#' -> false
                | _ when grid[a][b] = '.' -> true
                (*| (-1,0) when grid[a][b] = '^' -> true
                | (1,0) when grid[a][b] = 'v' -> true
                | (0,1) when grid[a][b] = '>' -> true
                | (0,-1) when grid[a][b] = '<' -> true*)
                | _ -> true


    let dist = new Dictionary<int*int,int>()
    let visited = new HashSet<int*int>()
    dist[(0,1)] <- 0

    let rec bfs queue =
        match queue with
        | [] -> ()
        | (i,j)::_ when (i,j) = (n-1, m-2) -> ()
        | u::rest ->
            if visited.Contains u then
                bfs rest
            else
                visited.Add u |> ignore
                let adj =
                    [(1,0); (-1,0); (0,-1); (0,1)]
                    |> List.filter (fun diff -> valid diff u)
                    |> List.map (fun diff -> diff ++ u)
                    |> List.map 
                        (fun v -> 
                                            if dist[u]+1 > dist.GetValueOrDefault(v,0) then
                                                dist[v] <- (dist[u]+1)
                                                v
                                            else
                                                (0,0))
                bfs (rest@adj)

    bfs [(0,1)]
    
    for i in 0 .. n - 1 do
        for j in 0 .. m - 1 do
            let c = dist.GetValueOrDefault((i,j),-1)
            printf "%02d " c
        printfn ""


(*let pt11 () = 
    let grid = 
        File.ReadAllLines("/home/nicholas/Documents/git/AoC/2023/AoC/AoC/day23.txt") 
        |> Array.map _.ToCharArray()
    
    let n = grid.Length
    let m = grid[0].Length

    let rec dfs (visited:HashSet<int*int>) (d:int) x = 
        if x = (n - 1, m - 2) then
            d
        else
            visited.Add x |> ignore
            let max = 
                seq{
                    for diff in [(1,0); (-1,0); (0,-1); (0,1)] do 
                        let (a,b) = diff++x
                        if a >= 0 || a < n || b >= 0 && b < m && not (visited.Contains (a,b)) then
                            yield dfs visited (d+1) (a,b)
                } |> Seq.max
            visited.Remove x |> ignore
            max
                    
    let l = dfs (new HashSet<int*int>()) 0 (0,1)
    printfn $"{l}"*)
pt11()