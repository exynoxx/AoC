open System
open System.IO
let file = File.ReadLines("input13.txt");;

type ParseResult<'a> =
   | SuccessResult of 'a * string
   | Failure of string

let ParseResultToOption x = match x with
| SuccessResult (x,_) -> Some(x)
| Failure _ -> None

let ParseResultContentToList x = match x with
| SuccessResult (ret,r) -> SuccessResult ([ret],r)
| Failure s -> Failure s
let IsNotNumber s = not (Char.IsDigit s)
let IntParser (s:string) =
    match Seq.tryFindIndex IsNotNumber s with
    | Some(i) -> match i with
        | 0 -> Failure s
        | _ -> SuccessResult(Int32.Parse s.[0..i], s.[i..])
    | _ -> SuccessResult(Int32.Parse s, "")
 
let Token (t:string) (s:string)  = match s.StartsWith(t) with
    | true -> SuccessResult (t, s.Substring(t.Length))
    | _ -> Failure s

let rec Rep f (s:string) = match f s with
    | SuccessResult (r, rest) -> SuccessResult (r, rest)::Rep f rest
    | Failure s -> [Failure s]
    
let rec Seq list s = match List.rev list with
    | f::fs ->
        match Seq fs s with
        | SuccessResult(l, rest) -> match f rest with
            | SuccessResult(ret, final) -> SuccessResult([ret]::l, final)
            | Failure rest -> Failure rest
        | Failure rest -> Failure rest
    | [] -> SuccessResult([],s)

let inline (||) (f:string->ParseResult<'a>) (g:string->ParseResult<'a>) (s:string) =
    match f s with
    | Failure _ -> g s
    | x -> x

let inline (&&) (f:'x->ParseResult<'a>) (g:'y->ParseResult<'b>) (s:string):ParseResult<Object list> =
    match f s with
    | SuccessResult(a,rest) -> match g rest with
        | SuccessResult(b, final) -> SuccessResult([a;b], final)
        | x -> x
    | x -> x 

let ArrayParser = (Token "[") && (Token "]")
    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
(*let (|Int|_|) s = match System.Int32.TryParse(s:string) with | true,i -> Some(i) | _ -> None
let Join s = s |> string.Join ""

type node =
    | File of int
    | Sym of string
    | Folder of node list

type IDictionary<'K,'V> with
    member x.GetKey k =
        let mutable out
        if x.TryGetKey(k,&out) then out else [] end

let f lines =
    let dict = Dictionary<string,node list>();
    let mutable current = ["/"]
    for line in lines do
        let k = Join current
        current <- match line with
            | "$"::"cd"::"/"::_ ->
                [current.Head]
            | "$"::"cd"::".."::_ ->
                current.Tail
            | "$"::"cd"::x::_ ->
                x::current
            | Int i::_ ->
                dict[k] = dict.GetKey(k) @ [File(i)]
                current
            | "dir"::d::_ ->
                dict[k] = dict.GetKey(k) @ [Sym(d)]
                current
            | _ -> 
                current
    dict
    

let rec BuildTreeSeq path =
    dict[path] |> List.Map (x =>
                            match x with
                                | Sym(d) -> BuildTree (d + path)
                                | x -> x)
    
     
let tree = BuildTree "/"*)