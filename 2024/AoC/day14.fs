module day14

open Utils
open System.IO
open System.Collections.Generic

let w = 101
let h = 103

let wraparound i n = (i % n + n) % n

let translate (sec:int) = 
    function 
    | ((i,j), (x,y)) -> (wraparound (i+x*sec) w, wraparound (j+y*sec) h)
    | _ -> failwith ""

let f = File.ReadAllLines("data/day14.txt")
        |> Array.map (TupleOf " ")
        |> Array.map (fun (p,v) -> IntTupleOf "," p[2..], IntTupleOf "," v[2..])
        |> Array.map (translate 100)
        |> Array.filter (fun (i,j) -> i <> (w/2) && j <> (h/2))

let quadrants = Dictionary<bool*bool, int>()
for (i,j) in f do
    let k = (i > (w/2), j > (h/2))
    quadrants[k] <- quadrants.GetValueOrDefault(k,0) + 1

let result = quadrants.Values |> Seq.reduce (*)
printfn "pt1 %i" result