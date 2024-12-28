module day4

open Utils
open System.IO
open System.Collections.Generic

let rawgrid = ParseGrid("Data/day4.txt")

let hp = [|for _ in 0..rawgrid[0].Length do [|'.'|]|]
let vp = [|'.';'.';'.'|]
let grid = 
    Array.collect id [| hp;hp;hp;rawgrid;hp;hp;hp |]
    |> Array.map (fun row -> Array.collect id [|vp; row; vp|])

let check (adj: char list) = 
    adj.ToString().Contains "XMAS" ||
    adj |> List.rev |> (fun x -> x.ToString().Contains "XMAS")

let checkcount (adj: char list) : int = if check adj then 1 else 0

let mutable result = 0
for i in 0..grid[0].Length do
    for j in 0..grid.Length do
        if grid[i][j] <> '.' then 
            let a = checkcount [grid[i-3][0];grid[i-2][0];grid[i-1][0];grid[i][j];grid[i+1][0];grid[i+2][0];grid[i+3][0]]
            let b = checkcount [grid[0][j-3];grid[0][j-2];grid[0][j-1];grid[i][j];grid[0][j+1];grid[0][j+2];grid[0][j+3]]
            let c = checkcount [grid[i-3][j-3];grid[i-2][j-2];grid[i-1][j-1];grid[i][j];grid[i+1][j+1];grid[i+2][j+2];grid[i+3][j+3]]
            let d = checkcount [grid[i+3][j-3];grid[i+2][j-2];grid[i+1][j-1];grid[i][j];grid[i-1][j+1];grid[i-2][j+2];grid[i-3][j+3]]
            result <- result + a+b+c+d
            //diagonals wrong
printfn "%i" result