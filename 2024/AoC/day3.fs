module day3

open System.IO
open System.Text.RegularExpressions

let f = File.ReadAllLines("Data/day3.txt") |> String.concat ""


let pt1() = 
    let result = 
        Regex("mul\((\d+),(\d+)\)").Matches(f)
        |> Seq.cast<Match>
        |> Seq.map (fun m ->
            let group1 = m.Groups[1].Value
            let group2 = m.Groups[2].Value
            int group1 * int group2
        )
        |> Seq.sum

    printfn "pt1 %i" result

type Ins = 
    | Mul of int
    | Do
    | Dont

let pt2() = 

    let mapMatch (m:Match) = 
        match m with
        | _ when m.Value.StartsWith("mul") -> Mul (int m.Groups[1].Value * int m.Groups[2].Value)
        | _ when m.Value.StartsWith("don't") -> Dont
        | _ when m.Value.StartsWith("do") -> Do

    let rec filter enabled = 
        function
        | [] -> 0
        | Do::xs -> filter true xs
        | Dont::xs -> filter false xs
        | Mul x::xs when enabled -> x + filter enabled xs
        | Mul _::xs when not enabled -> filter enabled xs

    let result = 
        Regex("mul\((\d+),(\d+)\)|do\(\)|don't\(\)").Matches(f)
        |> Seq.cast<Match>
        |> List.ofSeq
        |> List.map mapMatch
        |> filter true

    printfn "pt2 %i" result

pt1()
pt2()