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
    let mutable sum = 0L
    let mutable cubesInSum = new HashSet<_>()

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

            if hyperCube.Values |> all positive then 

                sum <- sum + int64(width hyperCube["x"])*int64(width hyperCube["m"])*int64(width hyperCube["a"])*int64(width hyperCube["s"])
                for c in cubesInSum do
                    let intersectee = intersection c hyperCube
                    if hyperCube.Values |> all positive then 
                        sum <- sum - int64(width intersectee["x"])*int64(width intersectee["m"])*int64(width intersectee["a"])*int64(width intersectee["s"])

                cubesInSum.Add hyperCube |> ignore