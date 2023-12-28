module day19

open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic

type actualPart = {x:int;m:int;a:int;s:int}

type RuleType = 
    | GTRule of string*int
    | LTRule of string*int
    | Default

type Rule = {typ:RuleType;goto:string}

let (|Rule|_|) input = 
    let m = Regex.Match(input,"(\w)(>|<)(\d+):(\w+)")
    if m.Success then
        let prop = m.Groups.Item(1).Value
        let cmp = m.Groups.Item(2).Value
        let num = m.Groups.Item(3).Value |> int
        let goto = m.Groups.Item(4).Value
        match cmp with
        | ">" -> Some {typ=GTRule (prop,num);goto=goto}
        | "<" -> Some {typ=LTRule (prop,num);goto=goto}
    else 
        None

let (|RuleDefault|_|) input =
    let m = Regex.Match(input,"\w+")
    if m.Success then Some {typ = Default;goto=m.Value} else None

let (|WorkFlow|_|) input =
    let stringToTyp = function
    | Rule r -> r
    | RuleDefault r -> r

    let m = Regex.Match(input, "(\w+){(.+)}")
    if m.Success then 
        let body = m.Groups.Item(2).Value.Split(',') |> Array.map stringToTyp
        Some (m.Groups.Item(1).Value, body) 
    else None

let pt1() = 

    let rec toF s prevf = 
        match s with
        | {typ= GTRule (prop,num);goto=goto} -> 
            match prop with 
            | "x" -> fun part -> if part.x > num then goto else prevf(part)
            | "m" -> fun part -> if part.m > num then goto else prevf(part)
            | "a" -> fun part -> if part.a > num then goto else prevf(part)
            | "s" -> fun part -> if part.s > num then goto else prevf(part)
        | {typ= LTRule (prop,num);goto=goto} -> 
            match prop with 
            | "x" -> fun part -> if part.x < num then goto else prevf(part)
            | "m" -> fun part -> if part.m < num then goto else prevf(part)
            | "a" -> fun part -> if part.a < num then goto else prevf(part)
            | "s" -> fun part -> if part.s < num then goto else prevf(part)
        |  {typ= Default;goto=goto} -> fun _ -> goto

    let (|Part|_|) input = 
        let m = Regex.Match(input, "{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}")
        if m.Success then 
            Some {
                x= m.Groups.Item(1).Value |> int;
                m= m.Groups.Item(2).Value |> int;
                a= m.Groups.Item(3).Value |> int;
                s= m.Groups.Item(4).Value |> int;
            } 
        else None

    let workflows = new Dictionary<_,_>()
    let mutable parts = []

    for s in File.ReadAllLines("day19.txt") do
        match s with
        | WorkFlow (name, rules) -> 
            let compositLambda = Seq.foldBack toF rules (fun _->"dummy")
            workflows[name] <- compositLambda
        | Part p -> parts <- p::parts
        | _ -> ()

    let rec eval part flow = 
        match flow with
        | "A" | "R" -> flow
        | flow -> eval part (workflows[flow](part))
            
    let accept = 
        parts
        |> List.filter (fun part -> (eval part "in") = "A")
        |> List.sumBy (fun part -> part.x + part.m + part.a + part.s)

    printfn $"{accept}"

let pt2() = 
    let workflows = new Dictionary<_,_>()
    for s in File.ReadAllLines("day19.txt") do
        match s with
        | WorkFlow (name, rules) -> workflows[name] <- rules
        | _ -> ()

    let InvertRule = function
        | {goto=goto;typ=GTRule (prop,num)} -> {typ = LTRule (prop,num+1);goto=goto}
        | {goto=goto;typ=LTRule (prop,num)} -> {typ = GTRule (prop,num-1);goto=goto}
        | x -> x

    let width (a,b) = b - a + 1
    let positive (a,b) = a < b
    let dimensionIntersect (a1,b1) (a2, b2) = (max a1 a2, min b1 b2)
    let cubeVolume (cube:Dictionary<string,int*int>) =
        cube.Values |> Seq.map width |> Seq.map uint64 |> Seq.reduce (*)

    let tighten rule (cube:Dictionary<string,int*int>) = 
        let newCube = new Dictionary<_,_>(cube)
        match rule.typ with
        | GTRule (prop,num) -> 
            newCube[prop] <- dimensionIntersect cube[prop] (num+1,4000)
            newCube
        | LTRule (prop,num) ->
            newCube[prop] <- dimensionIntersect cube[prop] (1,num-1)
            newCube
        | Default -> newCube
    
    let mutable sum = 0uL
    let rec dfs flow (cube:Dictionary<string,int*int>) =
        match flow with
        | "A" when cube.Values |> Seq.forall positive -> 
            sum <- sum + cubeVolume cube
        | x when x <> "R" -> 
            let mutable prevWithRulesInverted = cube
            for rule in workflows[flow] do
                dfs rule.goto (tighten rule prevWithRulesInverted)
                prevWithRulesInverted <- tighten (InvertRule rule) prevWithRulesInverted
        | _ -> ()

    let defaultConstraint = new Dictionary<_,_>()
    defaultConstraint["x"]<-(1,4000)
    defaultConstraint["m"]<-(1,4000)
    defaultConstraint["a"]<-(1,4000)
    defaultConstraint["s"]<-(1,4000)

    dfs "in" defaultConstraint
    printfn $"{sum}"

pt1()
pt2()