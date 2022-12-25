open System
open System.IO
open AoC
open AoC.ParserCombinator
open ParserCombinator

type Item =
    | Int of Int32
    | Array of Item list   


let intItem : Parser<Item> = IntParser |> map (Int)
let intAndComma : Parser<Item> = (intItem && Token ",") |> map fst

let arrayItem : Parser<Item> = (Token "[" && rep0 (intAndComma || intItem) && Token "]") ^^ (fun ((_, l),_) -> Array l)

let rec sum (x:Item) = match x with
| Int i -> i
| Array xs -> xs |> Seq.map sum |> Seq.sum 

let tree : Item = match run arrayItem "[1,2,3]" with
| Success (x,_) -> x
| err -> Int -1

printfn $"%d{sum tree}\n"


///let file = File.ReadLines("input13.txt")
