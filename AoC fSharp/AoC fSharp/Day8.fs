namespace AoC

open System
open System.Collections.Generic
open System.IO
open Util

module Day8 =
    
    let parse list = list
                    |> Seq.mapi (fun i line -> line |> Seq.mapi (fun j c -> KeyValuePair((i,j), int c - int '0')))
                    |> Seq.concat
                    |> Dictionary
    
    let directions = [(-1,0);(1,0);(0,-1);(0,1)]
        
    let pt1 () =
        let raw = File.ReadLines("../../../input8.txt")
        let graph = parse raw
        
        let rec dfs (graph:Dictionary<int*int,int> ref) height pos dir =
            if (graph.Value.ContainsKey pos) |> not then
                true
            elif graph.Value[pos] >= height then
                false
            else
                dfs graph height (pos++dir) dir
            
        let visible p h = directions |> Seq.map ( fun dir -> dfs (ref graph) h (p++dir) dir) |> Seq.contains true
        let count = graph.Keys |> Seq.filter (fun k -> visible k graph[k]) |> Seq.length
        printfn $"pt1 {count}"

    let pt2 () =
        let raw = File.ReadLines("../../../input8.txt")
        let graph = parse raw
        
        let rec ScenicScore1D (graph:Dictionary<int*int,int> ref) height pos dir =
            if not (graph.Value.ContainsKey pos) then
                0
            elif graph.Value[pos] >= height  then
                1
            else
                1 + ScenicScore1D graph height (pos++dir) dir
                
        let ScenicScore p h = directions
                              |> Seq.map ( fun dir -> ScenicScore1D (ref graph) h (p++dir) dir)
                              |> Seq.product
        
        let max = graph.Keys
                  |> Seq.map (fun k -> ScenicScore k graph[k])
                  |> Seq.max
     
        printfn $"pt2 {max}"

