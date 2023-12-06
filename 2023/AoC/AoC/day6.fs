module day6

open System.Text.RegularExpressions
open System.IO

let parse regexExtractor = 
    let lines = File.ReadAllLines("day6.txt") |> List.ofSeq
    let times = regexExtractor lines[0]
    let distances = regexExtractor lines[1]
    times, distances

let distance t hold = (t-hold) * hold

let howManyWays (t, record) = 
    seq {0L .. t} 
    |> Seq.map (fun hold -> distance t hold) 
    |> Seq.filter (fun dist -> dist>record)
    |> Seq.length

let pt1() = 
    let extractIntegers (input: string) = Regex("\d+").Matches(input) |> Seq.map (fun m -> int64 m.Value)
    let times, distances = parse extractIntegers
    let races = Seq.zip times distances
    let answer = races |> Seq.map howManyWays |> Seq.reduce (*)
    printfn $"{answer}"

let pt2() = 
    let extractOneIntegers (input: string) = Regex("\d+").Matches(input) |> Seq.map _.Value |> Seq.reduce (+) |> int64
    let time, distance = parse extractOneIntegers
    printfn $"{howManyWays (time,distance)}"
pt1()
pt2()