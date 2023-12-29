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

    let boxHead = new Dictionary<int, int>()
    let position = new Dictionary<string,int*int*int>()

    for e in items do
        match e with 
        | Addition (label, focal) ->
            let box = hashString label

            let slot = 
                match position.TryGetValue label with
                | true, (_, slot,_) -> slot
                | false, _ -> boxHead.GetValueOrDefault(box,0)
            
            position[label] <- (box,slot,focal)
            boxHead[box] <- slot+1

        | Deletion label -> position.Remove label |> ignore
        | _ -> failwith ""

    let focusPower boxlist = 
        boxlist
        |> Seq.sortBy (fun (_,slot,_)->slot)
        |> Seq.mapi (fun i (box,_,focal) -> (box+1)*(i+1)*focal)

    let l = 
        position.Values 
        |> Seq.groupBy (fun (box,_,_) -> box)
        |> Seq.map snd
        |> Seq.map focusPower
        |> Seq.collect id
        |> Seq.sum

    let answer = l
    printfn $"{answer}"


pt2()