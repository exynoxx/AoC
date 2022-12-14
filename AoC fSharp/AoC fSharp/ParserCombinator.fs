namespace AoC

open System

module ParserCombinator =
    type ParseResult<'a> =
        | Success of 'a
        | Failure of string

    type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

    let Token (k: string) =
        let inner (s: string) =
            match s.StartsWith(k) with
            | true -> Success(k, s.Substring(k.Length))
            | _ -> Failure s

        Parser inner

    let IsNotNumber s = not (Char.IsDigit s)

    let IntParser =
        let inner (s: string) =
            match Seq.tryFindIndex IsNotNumber s with
            | Some i ->
                match i with
                | 0 -> Failure s
                | _ ->
                    let result = Int32.Parse s.[0..i-1]
                    Success(result, s.[i..])
            | None -> Failure s

        Parser inner

    let run parser input =
        let (Parser innerFn) = parser
        innerFn input

    let (&&>) (p1:Parser<'a>) (p2:Parser<'b>) =
        let inner (s: string) =
            match run p1 s with
            | Success (result1, rest) ->
                match run p2 rest with
                | Success (result2, rest2) -> Success((result1, result2), rest2)
                | Failure err -> Failure err
            | Failure err -> Failure err

        Parser inner

    let (|||) p1 p2 =
        let inner (s: string) =
            match run p1 s with
            | Success result -> Success result
            | Failure _ ->
                match run p2 s with
                | Success result -> Success result
                | Failure err -> Failure err

        Parser inner

    let choice parsers = List.reduce (|||) parsers

    let rep0 (p: Parser<'a>) : Parser<'a list> =
        let rec inner (s: string) : 'a list * string =
            match run p s with
            | Success (firstval, firstRest) ->
                let (results, rest) = inner firstRest
                (firstval :: results, rest)
            | Failure err -> ([], s)

        let pretty (s: string) = Success(inner s)
        Parser pretty

    let map (f: 'a -> 'b) (p: Parser<'a>) =
        let inner (s: string) =
            match run p s with
            | Success (value, rest) -> Success(f value, rest)
            | Failure err -> Failure err

        Parser inner
    let (^^) (p:Parser<'a>) (f: 'a -> 'b) = map f p

    let get = function
        | Success (parse, _)-> parse
        | Failure f -> raise (new Exception $"bad parse {f}")