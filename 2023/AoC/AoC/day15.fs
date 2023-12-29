module day15

open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions


let hash current c = ((current + (int c)) * 17) % 256
let hashString (s:string) = s.ToCharArray() |> Array.fold hash 0

let pt1() = 

    let answer = 
        File.ReadAllLines("/home/nicholas/Documents/git/AoC/2023/AoC/AoC/day15.txt")
        |> String.concat("")
        |> _.Split(',')
        |> Array.map hashString
        |> Array.sum

    printfn $"{answer}"

let pt2() = 
    let (|Addition|_|) (input:string) = 
        if input.Contains '=' then let [|w;d|] = input.Split('=') in Some (w, int d) else None
        
    let (|Deletion|_|) (input:string) = 
        if input.Contains '-' then Some (input.Split('-')[0]) else None

    let items = 
        File.ReadAllLines("/home/nicholas/Documents/git/AoC/2023/AoC/AoC/day15.txt")
        |> String.concat("")
        |> _.Split(',')

    let position = new Dictionary<int,Dictionary<string,int>>()
    let value = Dictionary<string,int>()
    let mutable head = 0

    let ensureCreated box = 
        if not (position.ContainsKey box) then 
            position[box] <- new Dictionary<_,_>()

    for e in items do
        match e with 
        | Addition (label, focal) ->            
            if not (value.ContainsKey label) then
                let box = hashString label
                ensureCreated box
                position[box][label] <- head
                head <- head + 1
            value[label] <- focal

        | Deletion label -> 
            if value.ContainsKey label then 
                value.Remove label |> ignore
                position[hashString label].Remove label |> ignore

        
    let mutable answer = 0
    for box in position.Keys do
        let boxlistsum = 
            position[box] 
            |> Seq.sortBy _.Value 
            |> Seq.map _.Key
            |> Seq.mapi (fun i label -> (box+1)*(i+1)*value[label])
            |> Seq.sum

        answer <- answer + boxlistsum

    printfn $"{answer}"

pt2()