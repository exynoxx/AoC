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

let pt1 () = 
    
    let find s =
        let rec inner u (tops:HashSet<int*int>) = 
            if height u = 9 then 
                tops.Add u |> ignore
                1
            else
                for dv in adj do
                    let v = u++dv
                    if height v = (height u) + 1 then 
                        inner v tops |> ignore

                tops.Count
        inner s (HashSet<int*int>())
    
    let result = tailHeads |> List.sumBy (find)
    printfn "pt1 %i" result


let pt2 () = 
    //TODO can be optimized
    let rec find u = 
        match height u with
        | 9 -> 1
        | hu -> 
            let candidates = 
                adj |> List.map (fun dv -> u++dv)
                    |> List.where (fun v -> height v = hu + 1 )

            match candidates with
            | [] -> 0
            | xs -> List.sumBy find xs

    let result = tailHeads |> List.sumBy (find)
    printfn "pt2 %i" result


pt1()
pt2()