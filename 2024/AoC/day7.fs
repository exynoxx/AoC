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
    | CONCAT of Token * Token
    | MULT
    | ADD
    | WILDCARD

let pt1() = 
    
    let interpreter (commands: Token list) = 
        let reducer (acc:int64) op_num = 
            match op_num with
            | (MULT, NUM x) -> acc * x
            | (ADD, NUM x) -> acc + x
            | _ -> acc

        let init = match commands[0] with | NUM x -> x
        let a = commands |> List.skip 1 |> List.pairwise 
        a |> List.fold reducer init

    let permutations length =
        let rec inner acc = 
            function
            | 0 -> [acc] 
            | n -> [MULT; ADD] |> List.collect (fun t -> inner (t :: acc) (n - 1))
        inner [] length
        
    let merge (numbers: int64 array) (ops: Token list) = 
         let m = ops |> List.mapi (fun i e -> [NUM numbers[i]; e] ) |> List.collect id
         m @ [Array.last numbers |> NUM]

    let correct (result:int64, numbers: int64 array) : bool = 
        let ops = permutations (numbers.Length-1)
        let eqs = ops |> List.map (fun op_list -> merge numbers op_list )
        eqs |> List.exists (fun ops -> interpreter ops = result)

    let result = equations 
                |> Array.where correct
                |> Array.sumBy fst

    printfn "pt1 %i" result

let pt2() = 
    
    let interpreter (commands: Token list) = 
        let reducer (acc:int64) op_num = 
            match op_num with
            | (MULT, NUM x) -> acc * x
            | (ADD, NUM x) -> acc + x
            | _ -> acc

        let init = match commands[0] with | NUM x -> x
        let a = commands |> List.skip 1 |> List.pairwise 
        a |> List.fold reducer init

    let permutations length =
        let rec inner acc = 
            function
            | 0 -> [acc] 
            | n -> [MULT; ADD] |> List.collect (fun t -> inner (t :: acc) (n - 1))
        inner [] length
        
    let merge (numbers: int64 array) (ops: Token list) = 
         let m = ops |> List.mapi (fun i e -> [NUM numbers[i]; e] ) |> List.collect id
         m @ [Array.last numbers |> NUM]

    let correct (result:int64, numbers: int64 array) : bool = 
        let ops = permutations (numbers.Length-1)
        let eqs = ops |> List.map (fun op_list -> merge numbers op_list )
        eqs |> List.exists (fun ops -> interpreter ops = result)

    let result = equations 
                |> Array.where correct
                |> Array.sumBy fst

    printfn "pt2 %i" result



pt1()
pt2()