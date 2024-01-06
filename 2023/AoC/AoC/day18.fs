module day18

open System.IO
open utils

let shoelaceArea (n::g) = 
    List.pairwise(n::g@[n])
    |> List.map(fun ((x1,y1),(x2,y2))-> (x1*y2)-(y1*x2) )
    |> List.sum
    |> abs
    |> (fun x -> float x / 2.0)

let pickTheorem (internalArea:float) (boundrySize:int64) : float = internalArea + ((float boundrySize)/2.0) + 1.0
let area : ((int64*int64)list) -> int64 -> float = shoelaceArea >> pickTheorem

let pt1() = 

    let parse (s:string) = 
        let [|d;num;_|] = s.Split(' ')
        (char d, int64 num)

    let mutable boundryArea = 0L
    let getNext x (d, num) = 
        boundryArea <- boundryArea + num
        match d with
        | 'U' -> x++(-num,0L)
        | 'D' -> x++(num,0L)
        | 'L' -> x++(0L,-num)
        | 'R' -> x++(0L,num)

    let vertices = 
        File.ReadAllLines("day18.txt") 
        |> Array.map parse
        |> Array.fold (fun state e -> (getNext state.Head e)::state) [(0L,0L)] 
        |> List.tail

    let answer = area vertices boundryArea
    printfn $"{answer}"

let pt2() = 
    let parse (s:string) = 
        let [|_;_;lastpart|] = s.Split(' ')
        let d = lastpart[7]
        let hex = lastpart.Substring (2,lastpart.Length - 4)
        let length = System.Convert.ToInt64(hex,16)
        (d, length)

    let mutable boundryArea = 0L
    let getNext x (d, num) = 
        boundryArea <- boundryArea + num
        match d with
        | '3' -> x++(-num,0L)
        | '1' -> x++(num,0L)
        | '2' -> x++(0L,-num)
        | '0' -> x++(0L,num)

    let vertices = 
        File.ReadAllLines("day18.txt") 
        |> Array.map parse
        |> Array.fold (fun state e -> (getNext state.Head e)::state) [(0L,0L)]
        |> List.tail

    let answer = area vertices (boundryArea)
    printfn $"A {answer}"

pt1()
pt2()