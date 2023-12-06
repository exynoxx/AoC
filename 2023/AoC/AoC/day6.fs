module day6

open System.Text.RegularExpressions
open System.IO





let pt1() = 
    let extractIntegers (input: string) = Regex("\d+").Matches(input) |> Seq.map (fun m -> int m.Value)

    let parse () = 
        let lines = File.ReadAllLines("day6.txt") |> List.ofSeq
        let times = extractIntegers lines[0]
        let distances = extractIntegers lines[1]
        times, distances

    let distance t hold = (t-hold) * hold

    let howManyWays (t, record) = 
        seq {0 .. t} 
        |> Seq.map (fun hold -> distance t hold) 
        |> Seq.filter (fun dist -> dist>record)
        |> Seq.length

    let times, distances = parse()
    let races = Seq.zip times distances
    let answer = races |> Seq.map howManyWays |> Seq.reduce (*)
    printfn $"{answer}"

let pt2() = 
    let extractOneIntegers (input: string) = Regex("\d+").Matches(input) |> Seq.map _.Value |> Seq.reduce (+) |> int64

    let parse () = 
        let lines = File.ReadAllLines("day6.txt") |> List.ofSeq
        let time = extractOneIntegers lines[0]
        let distance = extractOneIntegers lines[1]
        time, distance

    let distance t hold = (t-hold) * hold

    let howManyWays (t, record) = 
        seq {0L .. t} 
        |> Seq.map (fun hold -> distance t hold) 
        |> Seq.filter (fun dist -> dist>record)
        |> Seq.length


    let time, distance = parse ()

    let answer = howManyWays (time,distance)
    printfn $"{answer}"
pt2()