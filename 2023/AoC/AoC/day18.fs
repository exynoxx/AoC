module day18

open System.IO
open System.Collections.Generic
open utils
open System.Text.Encodings


let pt1() = 
    let parse (s:string) = 
        let [|d;num;_|] = s.Split(' ')
        (d, int num)
    let instructions = File.ReadAllLines("day18.txt") |> Array.map parse

    let boundary = new HashSet<int*int>()

    let mutable s = (0,0)
    boundary.Add s
    for (d, num ) in instructions do
        for _ in 0 .. num - 1 do
            let ss = 
                match d with
                | "U" -> s++(-1,0)
                | "D" -> s++(1,0)
                | "L" -> s++(0,-1)
                | "R" -> s++(0,1)
            boundary.Add ss
            s <- ss

    let floodFill s = 
        let visited = new HashSet<int*int>()
        let q = new HashSet<int*int>()
        q.Add s

        let mutable valid = true
        while q.Count > 0 && valid do    
            let u = q |> Seq.find (fun _ -> true)
            q.Remove u
            visited.Add u

            for diff in [(0,1);(0,-1);(1,0);(-1,0)] do
                let v = u ++ diff
                if not (visited.Contains v || boundary.Contains v) then
                    q.Add v |> ignore
        visited.Count


    let rec laser prevOnBoundry (i,j) = 
        
        if j >= 5000 then 
            None
        elif boundary.Contains (i,j) && prevOnBoundry then 
            None
        elif boundary.Contains (i,j) then
            laser true (i,j+1)
        elif prevOnBoundry then 
            Some (i,j)
        else
            laser false (i,j+1)

    let rec findStart = 
        function
        | [] -> failwith ""
        | y::ys -> 
            match (laser false (y, -5000)) with 
            | Some s -> s
            | _ -> findStart ys

    let s = findStart [-10 .. 10]
    let area = boundary.Count + floodFill s
    printfn $"{area}"

let pt2() = 
    let parse (s:string) = 
        let [|_;_;lastpart|] = s.Split(' ')
        let d = lastpart[7]
        let hex = lastpart.Substring (2,lastpart.Length - 4)
        let length = System.Convert.ToInt32(hex,16)
        (d, length)
    
    let instructions = File.ReadAllLines("day18.txt") |> Array.map parse

    let mutable s = (0,0)
    let maxx (a,b) (c,d) = 
        (max a d, max b d)

    let minn (a,b) (c,d) = 
        (min a d, min b d)

    let mutable minValues = s
    let mutable maxValues = s
        
    let mainBoundry = new HashSet<int*int>()
    for (d, num ) in instructions do
        for _ in 0 .. num - 1 do
            let ss = 
                match d with
                | '3' -> s++(-1,0)
                | '1' -> s++(1,0)
                | '2' -> s++(0,-1)
                | '0' -> s++(0,1)
            mainBoundry.Add ss
            minValues <- minn minValues ss
            maxValues <- maxx maxValues ss
            s <- ss

    let boundries = new Dictionary<int, HashSet<int*int>>()
    let addToBoundary (i,j) = 
        let bucket = j/1000
        if boundries.ContainsKey bucket |> not then 
            boundries[bucket]<-new HashSet<int*int>()

        boundries[bucket].Add (i,j)

    mainBoundry |> Seq.forall addToBoundary
    mainBoundry.Clear()

    let floodFill (boundary:HashSet<_>) s = 
        let visited = new HashSet<int*int>()
        let q = new HashSet<int*int>()
        q.Add s

        let mutable valid = true
        while q.Count > 0 && valid do    
            let u = q |> Seq.find (fun _ -> true)
            q.Remove u
            visited.Add u

            for diff in [(0,1);(0,-1);(1,0);(-1,0)] do
                let v = u ++ diff
                if not (visited.Contains v || boundary.Contains v) then
                    q.Add v |> ignore

        boundary.Count + visited.Count

  
    let placeBoundry (boundary:HashSet<_>) y = 
        let (miny, maxy) = (snd minValues, snd maxValues)
        let rec inner write (i,j) = 
            if j > maxy then 
                ()
            elif (boundary.Contains s) then //beaware of horizontal boundry hit
                 inner (not write) (i,j+1)
            else
                if write then boundary.Add ((i,j)) |> ignore
                inner write (i,j+1)

        inner false (y, miny - 1)

    let BatchedFloodFill () = 
        let mutable globalArea = 0
        for i in boundries.Keys do
            placeBoundry boundries[i] (i*1000) //lower horizontal
            placeBoundry boundries[i] ((i+1)*1000) //upper horizontal

            let localArea = floodFill boundries[i] (0,0) //TODO
            globalArea <- globalArea + localArea
            boundries[i].Clear()
        globalArea

    let answer = BatchedFloodFill ()
    printfn $"{answer}"

pt2()