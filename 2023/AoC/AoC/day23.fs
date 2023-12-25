module day23

open System.IO
open System.Collections.Generic
open utils 

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


let pt2 () = 
    let grid = 
        File.ReadAllLines("/home/nicholas/Documents/git/AoC/2023/AoC/AoC/day23.txt") 
        |> Array.map _.ToCharArray()
    
    let n = grid.Length
    let m = grid[0].Length

    let valid (a,b) = 
        if a < 0 || a >= n || b < 0 && b >= m then
            false
        else
            match grid[a][b] with
            | '#' -> false
            | _ -> true

    let mutable globalMax = 0
    let rec bfs (queue:((int*int) * HashSetChain<int*int>) list) =
        match queue with
        | [] -> ()
        | (u,path)::rest ->

            if u = (n-1, m-2) then 
                globalMax <- max globalMax (path.size)
                printfn $"{globalMax}"

            path.Add u |> ignore

            let validNeighbors =
                [(1,0); (-1,0); (0,-1); (0,1)]
                |> List.map (fun diff -> diff ++ u)
                |> List.filter valid
                |> List.filter (fun v -> not (path.Contains v))

            match validNeighbors with
            | [] -> bfs rest
            | [v] -> bfs ((v,path)::rest)
            | vs -> 
                let elements = vs |> List.map (fun v -> (v,path.Branch()))
                bfs (elements@rest)


    let visited = HashSetChain<int*int> .Create()
    let _ = bfs [((0,1),visited)]

    printfn $"{globalMax}"

pt1()