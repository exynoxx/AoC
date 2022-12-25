open System
open System.Collections.Generic
open System.IO
open AoC.ParserCombinator

type Item =
    | Int of Int32
    | Array of Item list

let rec compare =
    function
    | x :: xs, y :: ys ->
        match x, y with
        | Int a, Int b when a > b -> false
        | Int a, Int b when a <= b -> compare (xs, ys)
        | Array _, Int _ -> compare (xs, Array [ y ] :: ys)
        | Int _, Array _ -> compare (Array [ x ] :: xs, ys)
        | Array a, Array b -> compare (a, b) && compare (xs, ys)
    | [], [] -> true
    | [], _ -> true
    | _ -> false

let intParser: Parser<Item> = IntParser ^^ Int
    
let intAndComma: Parser<Item> = (intParser &&> Token ",") ^^ fst


let mutable arrayParserRef = ref (Parser (fun x -> Failure "")) ///basicly null ptr
let wrappedArrayParser = Parser (fun (s:string) -> run (arrayParserRef.Value) s)
let rec arrayParser: Parser<Item> =
    let arrayAndComma = (wrappedArrayParser &&> Token ",") ^^ fst
    (Token "[" &&> rep0 (choice [intAndComma; intParser; arrayAndComma; wrappedArrayParser]) &&> Token "]") ^^ (fun ((_, l), _) -> Array l)

arrayParserRef <- ref arrayParser

let getPairs (source: IEnumerable<_>) =
    seq {
        use iter = source.GetEnumerator()

        while iter.MoveNext() do
            let first = iter.Current

            if iter.MoveNext() then
                let second = iter.Current
                let ret = (first, second)
                iter.MoveNext() |> ignore
                yield ret
    }

let solvePair x y =
    match run arrayParser x, run arrayParser y with
    | Success (xarray, _), Success (yarray, _) -> compare ([ xarray ], [ yarray ])
    | _ -> raise (new Exception $"bad parse {x} ;;; {y}")

File.ReadLines("../../../input13.txt")
|> getPairs
|> List.ofSeq
|> List.indexed
|> List.map (fun (i, (x, y)) -> (i + 1, solvePair x y))
|> List.filter snd
|> List.map fst
|> List.sum
|> printfn "%d"
