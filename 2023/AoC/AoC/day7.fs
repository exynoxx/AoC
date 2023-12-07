module day7

open System.IO
open System.Collections.Generic

let parse (s:string) = 
    let [|hand; bid|] = s.Split(' ')
    (int bid, hand.ToCharArray() |> Array.toList , s)


let translateChar c = 
    match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12 
    | 'J' -> 11
    | 'T' -> 10
    | x -> int x - int '0'


let pt1() =

    let primaryScore (letters: char list) = 
        let pairs = letters
                    |> Seq.groupBy id
                    |> Seq.map (fun (_, values) -> Seq.length values)
                    |> Seq.sortDescending 
                    |> List.ofSeq

        match pairs[0] with
        | 5 -> 6
        | 4 -> 5
        | 3 when pairs[1] = 2 -> 4
        | 3  -> 3
        | 2 when pairs[1] = 2 -> 2
        | 2 -> 1
        | _ -> 0


    let rec cmp2 (xletters:char list) (yletters:char list) i : int =
        match i with
        | 5 -> 0
        | _ -> 
            let x = translateChar xletters[i]
            let y = translateChar yletters[i]
            match compare x y with
            | 0 -> cmp2 xletters yletters (i+1)
            | x -> x

    let cmp (_,xletters,_) (_,yletters,_) : int = 
        let x = primaryScore xletters
        let y = primaryScore yletters

        match compare x y with
        | 0 -> cmp2 xletters yletters 0
        | x -> x
    
    let answer = File.ReadAllLines("day7.txt")
                |> Seq.map parse 
                |> Seq.sortWith cmp 
                |> Seq.mapi (fun i (bid,_,_) -> (i+1) * bid) 
                |> Seq.sum

    printfn $"A {answer}"

    
let pt2() =

    let primaryScore (letters: char list) = 
        let charCount = letters
                        |> Seq.groupBy id
                        |> Seq.map (fun (k, values) -> KeyValuePair.Create(k, Seq.length values))
                        |> Dictionary
        
        let js = if charCount.ContainsKey 'J' then charCount['J'] else 0
        charCount['J'] <- 0

        let pairs = charCount |> Seq.map _.Value |> Seq.sortDescending |> List.ofSeq
        match pairs[0]+js with
        | 5 -> 6
        | 4 -> 5
        | 3 when pairs[1] = 2 -> 4
        | 3  -> 3
        | 2 when pairs[1] = 2 -> 2
        | 2 -> 1
        | _ -> 0

    let rec cmp2 = function
        | ([],[]) -> 0
        | (x::xs, y::ys) -> 
            match compare (translateChar x) (translateChar y) with
            | 0 -> cmp2 (xs, ys)
            | x -> x

    let cmp (_,xletters,_) (_,yletters,_) : int = 
        let x = primaryScore xletters
        let y = primaryScore yletters

        match compare x y with
        | 0 -> cmp2 (xletters, yletters)
        | x -> x
    
    let answer = File.ReadAllLines("day7.txt")
                |> Seq.map parse 
                |> Seq.sortWith cmp 
                |> Seq.mapi (fun i (bid,_,_) -> (i+1) * bid) 
                |> Seq.sum

    printfn $"A {answer}"
pt1()
pt2()