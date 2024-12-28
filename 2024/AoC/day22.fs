module day22

open System.IO
open Utils
open System.Collections.Generic

let hashf =
       fun x -> (x*64UL ^^^ x) % 16777216UL
    >> fun x -> (x/32UL ^^^ x) % 16777216UL
    >> fun x -> (x*2048UL ^^^ x) % 16777216UL

let numbers =  File.ReadAllLines("data/day22.txt") |> Array.map uint64

let pt1 () = 
    let rec fn x = 
        function
        | 0 -> x
        | i -> fn (hashf x) (i-1)

    let result = numbers |> Array.sumBy (fun x -> fn x 2000)

    printfn "pt1 %i" result

let pt2 () = 

    let LastDigit x = x % 10UL

    let rec fn x result =
        function
        | 0 -> List.rev result
        | i -> 
            let h = hashf x
            fn h (h::result) (i-1)

    let inline quad_hash a b c d = 
        10_007L*a+1009L*b+101L*c+11L*d

    let banana_lookup x = 
        let secrets = fn x [] 2000
        let bananas = 
            secrets 
            |> List.map LastDigit 
            |> List.map int64
            |> Array.ofList

        let diffs= bananas |> Array.pairwise |> Array.map (fun (a,b) -> b-a)
        let lookup = Dictionary<int64, int64>()
        for i in 3..diffs.Length-1 do 
            let h = quad_hash diffs[i-3] diffs[i-2] diffs[i-1] diffs[i]
            lookup.TryAdd (h, bananas[i+1]) |> ignore
        lookup
    
    let max_banana = 
        numbers 
            |> Array.Parallel.map banana_lookup
            |> Dictionary.merge (+)
            |> _.Values
            |> Seq.max

    printfn "pt2 %i" max_banana

pt1()
pt2()