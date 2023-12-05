module day4

open System.Collections;
open System.Collections.Generic
open System
open System.IO;

let parseline (l:string) = 
        let [|game; cards|] = l.Split(':')
        let gameNum = game.Split(' ') |> Seq.last |> int
        let [|winstr; currentstr|] = cards.Split('|')
        let winner = winstr.Split(' ') |> Seq.filter (fun x -> x <> "") |> Seq.map int |> set
        let current = currentstr.Split(' ') |> Seq.filter (fun x -> x <> "") |> Seq.map int |> set
        gameNum, winner, current

let pt1() = 
    let calc game = 
        let _, win, current = parseline game
        let power = Set.intersect win current |> Set.count
        1 * (2.0f ** float32 (power-1) |> int)

    let finalsum = File.ReadAllLines("day4.txt") |> Seq.map calc |> Seq.sum

    printfn $"{finalsum}"

let pt2() = 

    let cards = File.ReadAllLines("day4.txt") 
                    |> Seq.map parseline  
                    |> Seq.map (fun (card, winner, current) -> KeyValuePair.Create (card,(winner,current)))
                    |> Dictionary

    let instances = cards.Keys  |> Seq.map (fun x -> KeyValuePair.Create (x,1)) |> Dictionary

    let compute game winner current = 
        let overlap = Set.intersect winner current |> Set.count
        for copy = game + 1 to game + overlap do
            instances[copy] <- instances[copy]+instances[game]
    
    for card in cards do
        let (win, curr) = card.Value
        compute card.Key win curr

    let sum = instances.Values |> Seq.sum
    printfn $"{sum}"
pt2()


