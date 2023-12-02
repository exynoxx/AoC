module day1


open System.IO
open System.Collections.Generic
open System
    
//let lines = File.ReadAllLines("input.txt")
//let (|Int|_|) (s: string) = try Some (int s) with | _ -> None


let numberWords = ["zero";"one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
let replacements = ["z1o";"o1e"; "t2o"; "t3e"; "f4r"; "f5e"; "s6x"; "s7n"; "e8t"; "n9e"]

let words = 
    Seq.zip numberWords replacements 
    |> Seq.map (fun (k,v) -> KeyValuePair(k,v))
    |> Dictionary<string, string>


let rec replaceWord (s:string) (word:string) = 
    match s.IndexOf(word) with
    | -1 -> s
    | i -> replaceWord $"{s[..i-1]}{words[word]}{s[i+word.Length..]}" word

let replaceWords (s:string) = 
    words.Keys
    |> Seq.fold (fun updated key -> replaceWord updated key) s

let firstlast array = $"{Seq.find Char.IsDigit array}{Seq.findBack Char.IsDigit array}" |> int

let solveLine s = 
    let a = replaceWords s
    let b = firstlast (a.ToCharArray())
    b |> int


let sum () = 
    let lines = File.ReadAllLines("input.txt")
    lines 
    |> Seq.map solveLine
    |> Seq.sum
    
let t = solveLine "11four11"
printfn $"{t}"
printfn $"{sum()}" 