open System
open System.Collections.Generic
open System.IO
open AoC.ParserCombinator

type Item =
    | Int of Int32
    | Array of Item list

let rec compareList = function
    | (x :: xs:Item list, y :: ys:Item list) ->
        match x, y with
        | Array a, Array b ->
            let cmp = compareList (a, b)
            if cmp = 0 then compareList (xs, ys) else cmp      
        | Int a, Int b when a = b -> compareList (xs, ys)
        | Int a, Int b when a < b -> -1
        | Int a, Int b when a > b -> 1
        | Array a, y -> compareList (a, [ y ])
        | x, Array b -> compareList ([ x ], b)
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1

let compare a b = compareList ([a],[b])


let intParser: Parser<Item> = IntParser ^^ Int

///basicly null ptr
let mutable arrayParserRef = ref (Parser(fun x -> Failure ""))

let wrappedArrayParser =
    let inner = fun (s: string) -> run (arrayParserRef.Value) s
    Parser(inner)

let rec arrayParser: Parser<Item> =
    let item = wrappedArrayParser ||| intParser
    let commaSepItem = (item &&> Token ",") ^^ fst ||| item

    (Token "[" &&> rep0 commaSepItem &&> Token "]") ^^ (fun ((_, l), _) -> Array l)

arrayParserRef <- ref arrayParser

let getPairs (source: IEnumerable<_>) =
    seq {
        use iter = source.GetEnumerator()

        while iter.MoveNext() do
            let first = iter.Current

            if iter.MoveNext() then
                let second = iter.Current
                yield (first, second)
                iter.MoveNext() |> ignore

    }

let solvePair x y =
    let tree1 = run arrayParser x
    let tree2 = run arrayParser y
    match tree1, tree2 with
    | Success (parse1, _), Success (parse2, _) -> compare parse1 parse2
    | _ -> raise (new Exception $"bad parse {x} ;;; {y}")

File.ReadLines("../../../input13.txt")
|> getPairs
|> List.ofSeq
|> List.indexed
|> List.map (fun (i, (x, y)) -> (i + 1, solvePair x y))
|> List.filter (fun (_,x) -> x < 0)
|> List.map fst
|> List.sum
|> printfn "%d"
