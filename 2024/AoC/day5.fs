module day5

open Utils
open System.IO
open System.Collections.Generic
open System.Diagnostics

let pairs (arr:int array) =
    [| for i in 0 .. arr.Length - 1 do
        for j in i + 1 .. arr.Length - 1 do
            yield (arr[i], arr[j]) |]

let median (arr:int array) = arr[arr.Length / 2]
let intlist (s:string) = s.Split "," |> Array.map int

let section1, section2 = 
    File.ReadAllLines("Data/day5.txt")
    |> Array.where (fun (s:string) -> s <> "") 
    |> Array.partition (fun (s:string) -> s.Contains ("|"))

let pt1() = 
    let inverseLookup = section1 |> Seq.map (fun s -> IntTuple "|" s ) |> Seq.map (fun (a,b)->(b,a)) |> HashSet
    let ordered (arr:int array) =  arr |> pairs |> Array.exists (fun pair -> inverseLookup.Contains pair) |> not

    let result = 
        section2
        |> Array.map intlist
        |> Array.where ordered
        |> Array.sumBy median

    printfn "pt1 %i" result


let pt2() = 
    let lookup = section1 
                 |> Seq.map (fun s -> IntTuple "|" s ) 
                 |> Seq.map (fun pair -> (pair, -1))
                 |> Seq.collect (fun ((a,b), order) -> [  ((a,b), order);  ((b,a), -order)])
                 |> Dict

    let unordered arr = arr |> pairs |> Array.exists (fun pair -> lookup[pair] > 0 )

    let result = 
        section2
        |> Array.map intlist
        |> Array.where unordered
        |> Array.map (fun arr -> arr |> Array.sortWith(fun a b -> lookup[(a,b)]))
        |> Array.sumBy median

    printfn "pt2 %i" result

let stopwatch = Stopwatch.StartNew()
pt1()
pt2()
printfn "%i" stopwatch.ElapsedMilliseconds