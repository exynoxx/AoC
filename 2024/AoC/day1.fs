module day1

open System.Collections;
open System.Collections.Generic
open System
open System.IO;
open Utils

let Tuple (s:string) = 
    match s.Split("   ") with
    | [|a;b|] -> (int a, int b)
    | _ -> failwith $"Not tuple {s}"

let f = File.ReadAllLines("data/day1.txt")

let pt1 () = 
    let a, b = f |> Array.map Tuple |> Array.unzip
    let a_sort = a |> Array.sort
    let b_sort = b |> Array.sort

    let distances = 
        Seq.zip a_sort b_sort 
        |> Seq.map (fun (a,b) -> abs(a-b))
        |> Seq.sum

    printfn "%i" distances

let pt2() = 
    let a, b = f |> Array.map Tuple |> Array.unzip
    let b_count = b |> Array.groupBy id |> Array.map (fun (k, list) -> (k, list.Length)) |> Dict
    let distances = a |> Seq.map (fun i -> i*b_count.GetValueOrDefault(i,0)) |> Seq.sum

    printfn "%i" distances


pt1()
pt2()