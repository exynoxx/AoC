module day21

open System.Collections.Generic
open System.IO
open System 
open utils


let swap (left : 'a byref) (right : 'a byref) =
    let temp = left
    left <- right
    right <- temp

let findStart (grid:char array array) = 
    
    let n = grid.Length
    let m = grid[0].Length

    let mutable s = (-1,-1)
    for i in 0 .. n - 1 do 
        for j in 0 .. m - 1 do 
            if grid[i][j] = 'S' then 
                s <- (i,j)

    s


let pt1() = 
    

    let grid = File.ReadAllLines("day21.txt") |> Array.map _.ToCharArray()
    let n = grid.Length
    let m = grid[0].Length

    let s = findStart grid

    let valid (v1,v2) = v1 >= 0 && v1 < n && v2 >= 0 && v2 < m && grid[v1][v2] <> '#'
        
    let mutable Q = new Queue<int*int>()
    let mutable QNext = new Queue<int*int>()
    let visited = new HashSet<int*int>()

    Q.Enqueue s
    let target = 64
    for k in 1 .. target do 
        for u in Q do 
            for v in [(0,1) ++ u;(0,-1) ++ u;(1,0) ++ u;(-1,0) ++ u] do
                if valid v && not (visited.Contains v) then
                    visited.Add v 
                    QNext.Enqueue v

        printfn $"{k} {QNext.Count}"

        swap (&Q) (&QNext)
        QNext.Clear()
        visited.Clear()
     
type CircularBuffer<'T> (size: int) =
    let mutable head = 0
    let mutable tail = 0
    let mutable buffer = Array.zeroCreate<'T> size

    member this.Enqueue (item: 'T) =
        buffer.[head] <- item
        head <- (head + 1) % size
        if head = tail then
            tail <- (tail + 1) % size

    member this.GetLast () = 
        let idx = (head - 1 + size) % size
        buffer.[idx]

    member this.GetFirst () =  buffer.[tail]


let inline (++++) (a: 'a * 'b * 'c * 'd) (b: 'a * 'b * 'c * 'd) : 'a * 'b * 'c * 'd =
    let (a1, b1, c1, d1) = a
    let (a2, b2, c2, d2) = b
    (a1 + a2, b1 + b2, c1 + c2, d1 + d2)

let pt2() = 
    let grid = File.ReadAllLines("day21.txt") |> Array.map _.ToCharArray()
    let n = grid.Length
    let m = grid[0].Length

    let s = findStart grid
    let valid (v1,v2) = grid[v1][v2] <> '#'

    let wrapAround (i,j) = ((i+n)%n, (j+m)%m)

    let mutable Q = new Queue<int*int>()
    let mutable QNext = new Queue<int*int>()
    //let visited = new HashSet<int*int>()

    Q.Enqueue s

    //let lastResults = new CircularBuffer<int>(3) // result of iteration k, k-1, k-2

    let record = new Dictionary<int*int,HashSet<int*int>>()
    let startEnd = new Dictionary<int*int, int*int>()

    let target = 500
    for k in 1 .. target do 
        
        for u in Q do 
            
            let ulocation:int*int = (if fst u <> 0 then n / abs(fst u) else 0, if snd u <> 0 then m / abs(snd u) else 0)
            record[ulocation].Add u

            let adj = [(0,1) ++ u;(0,-1) ++ u;(1,0) ++ u;(-1,0) ++ u]
            for (rv1,rv2) in adj do
                
                let i = if rv1 <> 0 then n / abs(rv1) else 0
                let v1 = (rv1+1000*n) % n

                let j = if rv2 <> 0 then n / abs(rv2) else 0
                let v2 = (rv2+1000*m) % m

                let v = (v1,v2)
                let location = (i,j)

                if not (record.ContainsKey location) then 
                    startEnd[location] <- (k,0)
                    record[location] <- new HashSet<_>()

                if valid v && not (record[location].Contains v) then
                    record[location].Add v 
                    if record[location].Count = 100000 then 
                        startEnd[location] <- (fst startEnd[location], k)
                    QNext.Enqueue (rv1,rv2)

        printfn $"{k} {QNext.Count}"
        
        swap (&Q) (&QNext)
        QNext.Clear()
pt2()





(*let pt2() = 
    let grid = File.ReadAllLines("day21.txt") |> Array.map _.ToCharArray()
    let n = grid.Length
    let m = grid[0].Length

    let (si,sj) = findStart grid
    let s = (si,0,sj,0)

    let valid (v1,_,v2,_) = grid[v1][v2] <> '#'

    let wrapDimension size (x,xx) = 
        match x with
        | neg when neg < 0 -> ((x+size)%size, xx-1)
        | pos when pos >= size -> (x%size, xx+1)
        | _ -> (x,xx)

    let wrapAround (i,ii,j,jj) = 
        let (_i,_ii) = wrapDimension n (i,ii)
        let (_j,_jj) = wrapDimension m (j,jj)
        (_i,_ii,_j,_jj)
        //((i+n)%n, (j+m)%m)

    let mutable Q = new Queue<int*int*int*int>()
    let mutable QNext = new Queue<int*int*int*int>()
    let visited = new HashSet<int*int*int*int>()

    Q.Enqueue s

    let lastResults = new CircularBuffer<int>(3) // result of iteration k, k-1, k-2
    let mutable snapShot = new HashSet<int*int>()

    let target = 200
    for k in 1 .. target do 
        
        for u in Q do 
            let adj = [(0,0,1,0);(0,0,-1,0);(1,0,0,0);(-1,0,0,0)] |> List.map (fun diff -> diff++++u |> wrapAround)
            for v in adj do
                if valid v && not (visited.Contains v) then
                    visited.Add v 
                    QNext.Enqueue v

        printfn $"{k} {QNext.Count}"
        lastResults.Enqueue QNext.Count            

(*        if k > 2 && lastResults.GetLast() = lastResults.GetFirst() && snapShot.Count = 0 then 
            snapShot <- new HashSet<int*int>(visited)
            printfn "snapshot"

        if snapShot.SetEquals(visited) then 
            ()
*)
        
        if k < target then 
            swap (&Q) (&QNext)
            QNext.Clear()
            visited.Clear()

    let map11 = visited |> Seq.filter (fun (_,ii,_,jj) -> ii = 1 && jj = 1) |> HashSet
    let map22 = visited |> Seq.filter (fun (_,ii,_,jj) -> ii = 2 && jj = 2) |> HashSet
    let map33 = visited |> Seq.filter (fun (_,ii,_,jj) -> ii = 3 && jj = 3) |> HashSet

    for _n1 in [-3;-2;-1;0;1;2;3] do
        for _m1 in [-3;-2;-1;0;1;2;3] do
            let a = visited |> Seq.filter (fun (_,ii,_,jj) -> ii = _n1 && jj = _m1) |> HashSet 
            for _n2 in [-3;-2;-1;0;1;2;3] do
                for _m2 in [-3;-2;-1;0;1;2;3] do
                    if _n1 <> _n2 && _m1 <> _m2 then
                        let b = visited |> Seq.filter (fun (_,ii,_,jj) -> ii = _n2 && jj = _m2) |> HashSet 
                        let result = a |> Seq.forall (fun (i,_,j,_) -> b.Contains ((i,_n2,j,_m2)))
                        if result then 
                            printfn $"{_n1} {_m1} {_n2} {_m2} - {abs (_n1-_n2)} {abs (_m1-_m2)}"
                        else 
                            printfn $"###################{_n1} {_m1} {_n2} {_m2} - {abs (_n1-_n2)} {abs (_m1-_m2)}"*)














(*
let visited = new HashSet<int*int>()
    let targets = new HashSet<int*int>()
    let target = 6

    let rec dfs u d = 

        if d = target then 
            targets.Add u |> ignore
        else 
            visited.Add u
        
            for diff in [(0,1);(0,-1);(1,0);(-1,0)] do
                let v = diff ++ u
                let (v1,v2) = v
                if v1 >= 0 && v1 < n && v2 >= 0 && v2 < m && grid[v1][v2]<>'.' && not (visited.Contains v) then
                    dfs v (d+1)

            visited.Remove u |> ignore

    dfs s 0
    printfn $"{targets.Count}"*)