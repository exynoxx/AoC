open System
open System.Collections.Generic
open System.IO
open AoC.ParserCombinator
open Microsoft.FSharp.Collections

module Day13 =
    type Item =
        | Int of Int32
        | Array of Item list

    let rec compare a b =
        match a, b with
        | Int x, Int y -> x.CompareTo(y)
        | Array (x::xs), Array (y::ys) ->
            match compare x y with
            | 0 -> compare (Array xs) (Array ys)
            | comp -> comp
        | Int x, Array y -> compare (Array [ a ]) b
        | Array x, Int y -> compare a (Array [ b ])
        | Array [], Array [] -> 0
        | Array [], _ -> -1
        | _, Array [] -> 1
        
    /// ########## PARSING ################
    let intParser: Parser<Item> = IntParser ^^ Int

    ///basicly null ptr
    let mutable arrayParserRef = ref (Parser(fun _ -> Failure ""))

    let wrappedArrayParser =
        let inner = fun (s: string) -> run (arrayParserRef.Value) s
        Parser(inner)

    let arrayParser: Parser<Item> =
        let item = wrappedArrayParser ||| intParser
        let commaSepItem = (item &&> Token ",") ^^ fst ||| item

        (Token "[" &&> rep0 commaSepItem &&> Token "]") ^^ (fun ((_, l), _) -> Array l)

    arrayParserRef <- ref arrayParser

    let pt1 () =
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
            let tree1 = run arrayParser x |> get
            let tree2 = run arrayParser y |> get
            compare tree1 tree2

        File.ReadLines("../../../input13.txt")
        |> getPairs
        |> Array.ofSeq
        |> Array.indexed
        |> Array.map (fun (i, (x, y)) -> (i + 1, solvePair x y))
        |> Array.filter (fun (_,x) -> x < 0)
        |> Array.sumBy fst
        |> printfn "%d"
    
    
    let pt2 =
        let cmp = (fun (_ ,x) (_, y) -> compare x y)
        let dividers = seq { yield "[[2]]"; yield "[[6]]"}
        let sorted = File.ReadLines("../../../input13.txt")
                    |> Seq.filter (fun s -> not (s = ""))
                    |> Seq.append dividers 
                    |> Seq.map (fun s -> (s,run arrayParser s |> get))
                    |> Array.ofSeq
                    |> Array.sortWith cmp
        let first = Array.findIndex (fun (s,_) -> s = "[[2]]") sorted
        let second = Array.findIndex (fun (s,_) -> s = "[[6]]") sorted
        let key = (first+1)*(second+1)
        printfn "key %i" key
    pt2 
