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

let interpreter (commands: Token list) = 
    let reducer (acc:int64) (op_num: Token*Token) = 
        match op_num with
        | (MULT, NUM x) -> acc * x
        | (CONCAT, NUM x) -> int64 $"{acc}{x}"
        | (ADD, NUM x) -> acc + x
        | _ -> acc

    let init = match commands[0] with | NUM x -> int64 x
    commands |> List.skip 1 |> List.pairwise |> List.fold reducer init

let permutations selection length =
    let rec inner acc n =
        seq {
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
        eqs |> Seq.exists (fun ops -> interpreter ops = result)

    let result = equations 
                |> Array.where correct
                |> Array.sumBy fst

    printfn "pt1 %i" result

let pt2() = 

    let correct (result:int64, numbers: int64 array) : bool = 
        let ops = permutations [ADD;MULT;CONCAT] (numbers.Length-1)
        let eqs = ops |> Seq.map (fun op_list -> merge numbers op_list )
        eqs |> Seq.exists (fun ops -> interpreter ops = result)

    let result = equations 
                |> Array.where correct
                |> Array.sumBy fst

    printfn "pt2 %i" result



pt1()
pt2()