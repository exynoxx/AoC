module day5

open Utils
open System.IO
open System.Collections.Generic

// ### HELPERS
//todo use Array.allpairs in utils
let pairs (arr:int array) =
    [| for i in 0 .. arr.Length - 1 do
        for j in i + 1 .. arr.Length - 1 do
            yield (arr[i], arr[j]) |]

let median (arr:int array) = arr[arr.Length / 2]
let intlist (s:string) = s.Split "," |> Array.map int


// ### ENTRY POINT
let s1, s2 = 
    File.ReadAllLines("Data/day5.txt")
    |> Array.where (fun s -> s <> "") 
    |> Array.partition (fun s -> s.Contains "|")

let section1 = s1 |> Array.map (IntTupleOf "|")
let section2 = s2 |> Array.map intlist

let pt1() = 
    let inverseLookup = section1 |> Seq.map (fun (a,b)->(b,a)) |> HashSet
    let ordered (arr:int array) =  arr |> pairs |> Array.any (fun pair -> inverseLookup.Contains pair) |> not

    let result = 
        section2
        |> Seq.where ordered
        |> Seq.sumBy median

    printfn "pt1 %i" result


let pt2() = 
    let lookup = section1 |> Seq.collect (fun (a,b) -> [  ((a,b), -1);  ((b,a), 1)]) |> Dict

    let invalid_ordered arr = arr |> pairs |> Array.any (fun pair -> lookup[pair] > 0 )

    let result = 
        section2
        |> Array.where invalid_ordered
        |> Array.map (fun arr -> arr |> Array.sortWith(fun a b -> lookup[(a,b)]))
        |> Array.sumBy median

    printfn "pt2 %i" result

pt1()
pt2()
