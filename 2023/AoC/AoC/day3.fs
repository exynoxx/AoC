module day3

open System.IO
open System;
open System.Collections.Generic

let grid = File.ReadAllLines("day3.txt")
        |> Seq.map _.ToCharArray()
        |> Array.ofSeq

let n = grid[0].Length
let m = grid.Length

let relativePositions =
        [ (-1, -1); (-1, 0); (-1, 1)
          (0, -1);           (0, 1)
          (1, -1);  (1, 0);  (1, 1) ]

let rec FindDigitSequence (i,a,b) =
    if a-1>=0 && Char.IsDigit grid[i].[a-1] then FindDigitSequence(i, a-1, b)
    else if b+1 < m && Char.IsDigit grid[i].[b+1] then FindDigitSequence(i, a, b+1)
    else (a,b)

let RecoverNumber (globalVisited:HashSet<int*int>) (i,j) = 
    if globalVisited.Contains (i,j) then 
        0
    else
        let a,b = FindDigitSequence(i, j,j) 
        let visited = seq {a .. b} |> Seq.map (fun j -> (i,j)) |> HashSet
        globalVisited.UnionWith visited
        new string(grid[i].[a..b]) |> int

let pt1 () =
    let globalVisited = new HashSet<int*int>()
    let VisiteSymbol (i,j) = relativePositions 
                            |> Seq.map (fun (a,b) -> (a+i,b+j))
                            |> Seq.filter (fun (a,b) -> Char.IsDigit grid[a].[b])
                            |> Seq.map (fun pos -> RecoverNumber globalVisited pos) 
                            |> Seq.sum

    let Solve () = 
        let mutable sum = 0
        for i in 0 .. n - 1  do    
            for j in 0 .. m - 1 do
                if not (Char.IsDigit grid[i].[j]) && grid[i].[j] <> '.' then
                    sum <- sum + VisiteSymbol (i,j)      
        sum

    Solve ()
    


let pt2 () =
    let globalVisited = new HashSet<int*int>()

    let VisiteGear (i,j) = 
        let numbers = relativePositions 
                    |> Seq.map (fun (a,b) -> (a+i,b+j))
                    |> Seq.filter (fun (a,b) -> Char.IsDigit grid[a].[b])
                    |> Seq.map (fun pos -> RecoverNumber globalVisited pos) 
                    |> Seq.filter (fun x -> x <> 0)
                    |> List.ofSeq

        if numbers.Length = 2 then
            numbers[0] * numbers[1]
        else 
            0

    
    let Solve () = 
        let mutable sum = 0
        for i in 0 .. n - 1  do    
            for j in 0 .. m - 1 do
                if grid[i].[j] = '*' then
                    sum <- sum + VisiteGear (i,j)      
        sum

    Solve ()
    
printfn $"{pt2()}"