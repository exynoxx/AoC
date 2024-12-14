module day11

open Utils
open System.Collections.Generic
open System.IO
open System.Diagnostics

let numbers = File.ReadAllText("Data/day11.txt") |> String.Split " " |> Array.int64

let inline numberOfDigits n = int (log10 (float n)) + 1

let inline split (x: int64) =
    let digits = numberOfDigits x
    let half = digits / 2
    let divisor = pown 10L half // Compute 10^half as the divisor
    let left = x / divisor
    let right = x % divisor
    left, right

let cache = Dictionary<int64*int64,int64>()
let rec f x d = 
    match x with
    | _ when d = 0 -> 1L
    | _ when cache.ContainsKey (x,d) -> cache[(x,d)]
    | 0L -> 
        let result = f 1 (d-1)
        cache[(x,d)] <- result
        result
    | x when numberOfDigits x % 2 = 0 -> 
        let a, b = split x
        let result = f a (d-1) + f b (d-1)
        cache[(x,d)] <- result
        result
    | _ -> 
        let result = f (x*2024L) (d-1)
        cache[(x,d)] <- result
        result

let pt1 () = printfn "pt1 %i" (Array.sumBy (fun x -> f x 25) numbers)
let pt2 () = printfn "pt2 %i" (Array.sumBy (fun x -> f x 75) numbers)

pt1()
pt2()