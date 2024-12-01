module day1

open System.Diagnostics
open System.Collections;
open System.Collections.Generic
open System
open System.IO;
open Utils

let pt1 () = 
    let stopwatch = Stopwatch.StartNew()
    let f = File.ReadAllLines("data/day1.txt")

    let a, b = f |> Array.map IntTuple |> Array.unzip
    let a_sort = a |> Array.sort
    let b_sort = b |> Array.sort

    let distances = 
        Array.zip a_sort b_sort 
        |> Array.map (fun (a,b) -> abs(a-b))
        |> Array.sum

    stopwatch.Stop()
    printfn "%i %i ms" distances stopwatch.ElapsedMilliseconds

let pt2() = 
    let stopwatch = Stopwatch.StartNew()
    let f = File.ReadAllLines("data/day1.txt")
    let a, b = f |> Array.map IntTuple |> Array.unzip
    let b_count = b |> Array.groupBy id |> Array.map (fun (k, list) -> (k, list.Length)) |> Dict
    let distances = a |> Array.map (fun i -> i*b_count.GetValueOrDefault(i,0)) |> Array.sum

    stopwatch.Stop()
    printfn "%i %i ms" distances stopwatch.ElapsedMilliseconds

pt1()
pt2()
