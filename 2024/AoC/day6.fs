module day6

open Utils
open System.Collections.Generic
open System.Collections

type BinaryTree<'T> =
    | Empty
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>

let rec createTree (sortedList: 'T list) =
    match sortedList with
    | [] -> Empty
    | _ ->
        let mid = List.length sortedList / 2
        let value = sortedList.[mid]
        let left = sortedList.[..mid - 1]
        let right = sortedList.[mid + 1..]
        Node(value, createTree left, createTree right)

let direction = [(-1, 0); (0,1); (1,0); (0,-1)]

let grid = ParseGrid("Data/day6.txt")

let getall c = 
    seq {
        for i in 0..grid.Length - 1 do
            for j in 0..grid[0].Length - 1 do
                if grid[i][j] = c then
                    yield (i,j)
    }


let s = getall '^' |> Seq.exactlyOne

let pt1() = 
    let visited = HashSet<int*int>()
    let rec dfs (i,j) d = 
        if i >= grid.Length || i >= grid[0].Length then 
            ()
        else if grid[i][j] = '#' then 
            dfs ((i,j)--direction[d]++direction[(d+1)%4]) ((d+1)%4)
        else
            visited.Add((i,j)) |> ignore
            dfs ((i,j)++direction[d]) d

    dfs s 0
    printfn "%i" visited.Count

let pt2() = 

    let rec search (a,b) tree =
        match tree with
        | Empty -> false
        | Node(v, left, right) ->
            if a <= v && v <= b then 
                true
            else if v < a then 
                search (a,b) left
            else
                search (a,b) right

    let hempty = [for _ in 0..grid.Length - 1 do List<int>()]
    let vempty = [for _ in 0..grid[0].Length - 1 do List<int>()]

    for (x,y) in getall '#' do
        hempty[x].Add(y)
        vempty[y].Add(x)

    let hbuckets = [for x in hempty do yield x |> List.ofSeq |> createTree]
    let vbuckets = [for x in vempty do yield x |> List.ofSeq |> createTree]

    let all = HashSet<int*int>()
    let paths = ResizeArray()

    let mutable obstructions = 0
    let placeObstruction () = obstructions<-obstructions+1

    let rec dfs (i,j) d (current:HashSet<int*int>) = 
        if i >= grid.Length || i >= grid[0].Length then 
            ()
        else if grid[i][j] = '#' then 
            paths.Add current
            dfs ((i,j)--direction[d]++direction[(d+1)%4]) ((d+1)%4) (HashSet<int*int>())
        else 
            
            match d with 
            | 0 | 2 when paths.Count >= 2 -> 
                //we go up/down
                match paths[paths.Count-2] |> Seq.where (fun (x,_) -> i = x) |> Seq.tryExactlyOne with
                | Some (_,y) -> 
                    let min,max = min y j, max y j
                    if search (min,max) hbuckets[i] then 
                        () 
                    else 
                       placeObstruction() 

                | _ -> ()

            | 1 | 3 when paths.Count >= 2 ->
                //we go left/right
                match paths[paths.Count-2] |> Seq.where (fun (_,y) -> j = y) |> Seq.tryExactlyOne with
                | Some (x,_) -> 
                    let min,max = min x i, max x i
                    if search (min,max) vbuckets[j] then 
                        () 
                    else 
                       placeObstruction() 
                | _ -> ()

            | _ -> ()

            if all.Contains (i,j) then
                placeObstruction()
            
            all.Add((i,j)) |> ignore
            current.Add((i,j)) |> ignore
            dfs ((i,j)++direction[d]) d current

    let result = dfs s 0 (HashSet<int*int>())
    printfn "%i" obstructions


pt1()
pt2()
