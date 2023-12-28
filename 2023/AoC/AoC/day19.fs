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

let pt2() = 
    let InvertRule rule = 
        let invtyp = 
            match rule.typ with
            | GTRule (prop,num) -> LTRule (prop,num+1)
            | LTRule (prop,num) -> GTRule (prop,num-1)
            | Default -> Default
        {typ=invtyp;goto=rule.goto}

    let width (a,b) = b-a
    let positive (a,b) = a<=b
    let all = Seq.forall
    let dimensionIntersect d1 d2 = 
        let (a1,b1) = d1
        let (a2, b2) = d2
        (max a1 a2, min b1 b2)

    let cubeVolume (cube:Dictionary<string,int*int>) = 
        let mutable local = 1uL
        for dimension in cube.Values do
            if positive dimension then
                let w = uint64(width dimension)
                local <- local * if w = 0uL then 1uL else w
        local

    let intersection (c1:Dictionary<string,int*int>) (c2:Dictionary<string,int*int>) = 
        let cube = new Dictionary<_,_>()
        cube["x"]<-dimensionIntersect c1["x"] c2["x"]
        cube["m"]<-dimensionIntersect c1["m"] c2["m"]
        cube["a"]<-dimensionIntersect c1["a"] c2["a"]
        cube["s"]<-dimensionIntersect c1["s"] c2["s"]
        cube

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
    let mutable sum = 0uL
    let mutable cubesInSum = new HashSet<_>()

    let rec dfs flow =
        if flow = "A" then
        
            let hyperCube = new Dictionary<_,_>(defaultConstraint)
            
            for rule in constraints do
                match rule.typ with
                | GTRule (prop,num) -> hyperCube[prop] <- dimensionIntersect hyperCube[prop] (num+1,4000)
                | LTRule (prop,num) -> hyperCube[prop] <- dimensionIntersect hyperCube[prop] (1,num-1)
                | Default -> ()

            if hyperCube.Values |> all positive then
                sum <- sum + cubeVolume hyperCube
                for c in cubesInSum do
                    let intersectee = intersection c hyperCube
                    if intersectee.Values |> all positive then
                        sum <- sum - cubeVolume intersectee

                cubesInSum.Add hyperCube |> ignore
            
        elif flow = "R" then 
            ()
        else

            let invertedrules = new HashSet<_>()
            for rule in workflows[flow] do
                constraints.Add rule
                dfs rule.goto
                constraints.Remove rule

                let inverted = InvertRule rule
                constraints.Add inverted
                invertedrules.Add inverted

            invertedrules |> Seq.forall constraints.Remove |> ignore

    dfs "in"
    printfn $"{sum}"
pt2()