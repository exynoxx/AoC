module day8

open Utils
open System.Collections.Generic

let grid = ParseGrid("data/day8.txt")

let signals = 
    seq {
        for i in 0..grid.Length - 1 do
            for j in 0..grid[0].Length - 1 do
                if grid[i][j] <> '.' then
                    yield (grid[i][j], i,j)
    } 
    |> Seq.groupBy (fun (x,_,_) -> x)
    |> Seq.map (
        fun (k, elements) -> 
            (k, elements |> Seq.map (fun (_,i,j) -> i,j) |> Array.ofSeq))
    |> Dict

let pairs (arr:'a array) =
    [| for i in 0 .. arr.Length - 1 do
        for j in i + 1 .. arr.Length - 1 do
            yield (arr[i], arr[j]) |]

(*for (k,v) in signals.Items () do 
    let (x,y), (i,j) = pairs v*)
    


(*if( bb.ix <= p.x && p.x <= bb.ax && bb.iy <= p.y && p.y <= bb.ay ) {
    // Point is in bounding box
}*)