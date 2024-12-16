module day10

open Utils
open System.Collections.Generic

let adj = [(0,1);(0,-1);(1,0);(-1,0)]
let grid = ParseIntGrid("data/day10.txt")
let height (i,j) = 
    if i >= 0 && i < grid.Length && j >= 0 && j < grid[0].Length then 
        grid[i][j]
    else
        -1

let tailHeads = [
    for i in 0..grid.Length - 1 do
        for j in 0..grid[0].Length - 1 do
            if grid[i][j] = 0 then
                yield (i,j) 
]


let cache = Dictionary<int*int,int>()

let rec f u (tops:HashSet<int*int>) =
    if height u = 9 then 
        tops.Add u
        1
    else if cache.ContainsKey u then 
        cache[u]
    else
        for dv in adj do
            let v = u++dv
            if height v = (height u) + 1 then 
                f v tops |> ignore

        cache[u] <- tops.Count 
        tops.Count

let mutable result = 0
for s in tailHeads do 
    result <- result + f s (HashSet<int*int>())

printfn "pt1 %i" result