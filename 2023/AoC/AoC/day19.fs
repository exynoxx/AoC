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

let parseBody (input:string) =
    input.Split(',') 
    |> Array.map 
        (fun rs -> 
                match rs with 
                | Rule r -> r
                | RuleDefault r -> r
    )

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

    for s in File.ReadAllLines("/home/nicholas/Documents/git/AoC/2023/AoC/AoC/day19.txt") do
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

pt1()

let pt2() = 

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

    let parseBody (input:string) =
        input.Split(',') 
        |> Array.map 
            (fun rs -> 
                    match rs with 
                    | Rule r -> r
                    | RuleDefault r -> r
        )

    let (|WorkFlow|_|) input =
        let m = Regex.Match(input, "(\w+){(.+)}")
        if m.Success then Some (m.Groups.Item(1).Value, parseBody (m.Groups.Item(2).Value)) else None

    let InvertRule rule = 
        let invtyp = 
            match rule.typ with
            | GTRule (prop,num) -> LTRule (prop,num+1)
            | LTRule (prop,num) -> GTRule (prop,num-1)
            | Default -> Default
        {typ=invtyp;goto=rule.goto}

    let workflows = new Dictionary<_,_>()
    for s in File.ReadAllLines("/home/nicholas/Documents/git/AoC/2023/AoC/AoC/day19.txt") do
        match s with
        | WorkFlow (name, rules) -> workflows[name] <- rules
        | _ -> ()

    let defaultConstraint = new Dictionary<_,_>()
    defaultConstraint["x"]<-(1,4000)
    defaultConstraint["m"]<-(1,4000)
    defaultConstraint["a"]<-(1,4000)
    defaultConstraint["s"]<-(1,4000)

    let constraints = new HashSet<Rule>()
    let mutable sum = 0L

    let rec dfs flow =
        if flow = "A" then
        
            let hyperCube = new Dictionary<_,_>(defaultConstraint)
            
            for rule in constraints do
                match rule.typ with
                | GTRule (prop,num) -> 
                    let (minn,maxx) = hyperCube[prop]
                    hyperCube[prop] <-(max minn num,maxx)
                | LTRule (prop,num) -> 
                    let (minn,maxx) = hyperCube[prop]
                    hyperCube[prop] <- (minn,min maxx num)
                | Default -> ()

            let (xa,xb) = hyperCube["x"]
            let (ma,mb) = hyperCube["m"]
            let (aa,ab) = hyperCube["a"]
            let (sa,sb) = hyperCube["s"]
            if xa<xb && ma<mb && aa < ab && sa < sb then
                sum <- sum + int64(xb-xa)*int64(mb-ma)*int64(ab-aa)*int64(sb-sa)

            //remove overlapping cubes

            
        elif flow = "R" then 
            ()
        else
            for rule in workflows[flow] do
                constraints.Add rule
                dfs rule.goto
                constraints.Remove rule
                let inverted = InvertRule rule
                constraints.Add inverted
                dfs rule.goto
                constraints.Remove inverted

    dfs "in"
    printfn $"{sum}"
//pt2()