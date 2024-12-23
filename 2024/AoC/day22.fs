module day22

open System.IO
open Utils
open System.Collections.Generic

let hashf =
       fun x -> (x*64UL ^^^ x) % 16777216UL
    >> fun x -> (x/32UL ^^^ x) % 16777216UL
    >> fun x -> (x*2048UL ^^^ x) % 16777216UL

let rec fn x = 
        function
        | 0 -> x
        | i -> fn (hashf x) (i-1)

let pt1 () = 
    let numbers = File.ReadAllLines("data/day22.txt")

    let result = 
        numbers 
        |> Array.map uint64
        |> Array.sumBy (fun x -> fn x 2000)

    printfn "pt1 %i" result

let pt2 () = 

    let LastDigit x = x % 10UL
    let rec unfold x = 
        function
        | 0 -> []
        | i -> 
            let h = hashf x
            h::unfold h (i-1)

    printfn "pt2 begin"

    let numbers =  File.ReadAllLines("data/day22.txt") |> Array.map uint64

    

    let banana_lookup x = 
        let secrets = unfold x 2000
        let bananas = 
            secrets 
            |> List.map LastDigit 
            |> List.map int64
            |> Array.ofList

        let diffs= bananas |> Array.pairwise |> Array.map (fun (a,b) -> b-a)
        let lookup = Dictionary<int64*int64*int64*int64, int64>()
        for i in 3..diffs.Length-1 do 
            let num_bananas = bananas[i+1]
            let changes = (diffs[i-3], diffs[i-2], diffs[i-1], diffs[i])

            lookup.TryAdd (changes, num_bananas) |> ignore
        lookup
    
    let max_banana = 
        numbers 
            |> Array.map banana_lookup
            |> Dictionary.merge (Seq.reduce (+))
            |> _.Values
            |> Seq.max

    printfn "pt2 %i" max_banana

//pt1()
pt2()