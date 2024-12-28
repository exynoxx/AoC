namespace AoC

open System
open System.Collections.Generic
open System.IO
open System.Security.Principal
open Util

module Day10 =
    
    type Ins =
        | Addx of int
        | Noop
    let listToIns (x:string[]) = if x[0] = "addx" then Addx (int x[1]) else Noop
    
    let instructions = File.ReadLines("../../../input10.txt")
                           |> Seq.map (fun x -> x.Split ' ')
                           |> Seq.map listToIns
                           |> List.ofSeq
        
    let rec generate (ins:Ins list) prev carry wait : int list =
        match wait with
        | 0 -> match ins with
                | Addx i::xs->
                    let current = prev+carry
                    current::generate xs current i 1
                | Noop::xs ->
                    let current = prev+carry
                    current::generate xs current 0 0
                | [] -> []
        | x -> prev::generate ins prev carry (x-1)
        
    let pt1 () =
        
        let valuesOverTime = generate instructions 1 0 1
        
        let sum = [20; 60; 100; 140; 180; 220]
                |> Seq.map (fun t -> t * valuesOverTime[t])
                |> Seq.sum
            
        printfn $"pt1 {sum}"
        
    let split length (xs: seq<'T>) =
        let rec loop xs =
            [
                yield Seq.truncate length xs |> Seq.toList
                match Seq.length xs <= length with
                | false -> yield! loop (Seq.skip length xs)
                | true -> ()
            ]
        loop xs
        
    let pt2 () =
        let GetPixel (i:int) (p:int) = if i = p || i = p+1 || i = p+2 then '#' else ' '

        let valuesOverTime = generate instructions 1 0 1
        let all = valuesOverTime |> Seq.indexed |> Seq.skip(1) |> Seq.map (fun (i,v) -> GetPixel (i%40) v) 
        
        let lines = all |> split 40 |> Seq.map String.Concat 
        printfn "pt2"
        for l in lines do
            printfn $"{l}"



        
        
  