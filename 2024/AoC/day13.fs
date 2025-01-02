module day13

open Utils
open System.IO
open System.Text.RegularExpressions

let (|Button|_|) (input:string) = 
    match regex "Button \w: X+(\d+), Y+(\d+)" input with 
    | Some [|x;y|] -> Some (int x, int y)
    | None -> None

let (|Prize|_|) (input:string) = 
    match regex "Prize: X=(\d+), Y=(\d+)" input with 
    | Some [|x;y|] -> Some (int x, int y)
    | None -> None


let f = 
    File.ReadAllLines("data/day13.txt")
    |> Array.where  (fun s -> s <> "")


let pt1() = 




    printfn "pt1 %i" 0
pt1()

