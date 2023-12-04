module day1


open System.IO
open System.Collections.Generic
open System
open System.Text.RegularExpressions
    
//let lines = File.ReadAllLines("input.txt")
//let (|Int|_|) (s: string) = try Some (int s) with | _ -> None

let theNumbers = "one|two|three|four|five|six|seven|eight|nine"
let regex = Regex($"(?=\d|{theNumbers})(\d|{theNumbers})")

let nums = 
    seq {
        ("one",1);
        ("two",2);
        ("three",3);
        ("four",4);
        ("five",5);
        ("six",6);
        ("seven",7);
        ("eight",8);
        ("nine",9);
    } |> dict |> Dictionary

let find s =
    let matchh = regex.Matches(s)
    let first = matchh.Item(0).Value
    let last = matchh.Item(matchh.Count-1).Value
    let f = if nums.ContainsKey first then nums[first] else int first
    let l = if nums.ContainsKey last then nums[last] else int last
    $"{f}{l}" |> int

let sum () = 
    let lines = File.ReadAllLines("input.txt")
    lines 
    |> Seq.map find
    |> Seq.sum
    
let t = find "twoneightwo123eighthreeight"
printfn $"{t}"
printfn $"{sum()}" 