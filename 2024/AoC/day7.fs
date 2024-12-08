module day7

open Utils
open System.IO
open System.Collections.Generic

let equations = 
    File.ReadAllLines("Data/day7.txt")
    |> Array.map (TupleOf ": ")
    |> Array.map (fun (result, numbers) -> (int64 result, numbers |> String.Split " " |> Array.map int64 ))

type Token = 
    | NUM of int64
    | CONCAT
    | MULT
    | ADD
    | WILDCARD

let interpreter (target: int64) (commands: Token list) = 

    let rec compute (acc:int64) = 
        function
        | [] -> acc
        | _ when acc > target -> acc //optimization 1
        | (NUM _, _) :: xs ->       compute acc xs
        | (MULT, NUM x) :: xs ->    compute (acc * x) xs
        | (CONCAT, NUM x) :: xs ->  compute (int64 $"{acc}{x}") xs
        | (ADD, NUM x) :: xs ->     compute (acc + x) xs

    let init = match commands[0] with | NUM x -> int64 x
    let pairs = commands |> List.skip 1 |> List.pairwise
    compute init pairs

let permutations selection length =
    let rec inner acc n =
        seq { //optimization 2
            match n with
            | 0 -> yield acc
            | _ -> 
                for t in selection do
                    yield! inner (t :: acc) (n - 1)
        }
    inner [] length

let merge (numbers: int64 array) (ops: Token list) = 
    let m = ops |> List.mapi (fun i e -> [NUM numbers[i]; e] ) |> List.collect id
    m @ [Array.last numbers |> NUM]

let pt1() = 
    let correct (result:int64, numbers: int64 array) : bool = 
        let ops = permutations [ADD;MULT] (numbers.Length-1)
        let eqs = ops |> Seq.map (fun op_list -> merge numbers op_list )
        eqs |> Seq.exists (fun ops -> interpreter result ops = result)

    let result = equations 
                |> Array.where correct
                |> Array.sumBy fst

    printfn "pt1 %i" result

let pt2() = 

    let correct (result:int64, numbers: int64 array) : bool = 
        let ops = permutations [ADD;MULT;CONCAT] (numbers.Length-1)
        let eqs = ops |> Seq.map (fun op_list -> merge numbers op_list )
        eqs |> Seq.exists (fun ops -> interpreter result ops = result)

    let result = equations 
                |> Array.where correct
                |> Array.sumBy fst

    printfn "pt2 %i" result

//pt3 DP where one starts backwards and recurses down until 1 left

pt1()
pt2()