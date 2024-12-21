module day14

open Utils
open System.IO
open System.Collections.Generic
open System
let w = 101
let h = 103

let wraparound i n = (i % n + n) % n
let translate (sec:int) ((i,j), (x,y)) = (wraparound (i+x*sec) w, wraparound (j+y*sec) h)

let pt1 () = 
    
    let result = 
        File.ReadAllLines("data/day14.txt")
        |> Array.map (TupleOf " ")
        |> Array.map (fun (p,v) -> IntTupleOf "," p[2..], IntTupleOf "," v[2..])
        |> Array.map (translate 100)
        |> Array.filter (fun (i,j) -> i <> (w/2) && j <> (h/2))
        |> Array.groupBy (fun (i,j) -> (i > (w/2), j > (h/2)))
        |> Array.map (fun (k, pos) -> pos.Length)
        |> Array.reduce (*)

    printfn "pt1 %i" result

let pt2 () = 
    let f = File.ReadAllLines("data/day14.txt")
            |> Array.map (TupleOf " ")
            |> Array.map (fun (p,v) -> IntTupleOf "," p[2..], IntTupleOf "," v[2..])

    use writer = new StreamWriter("data/day14out.txt", false) // false = overwrite mode
    for i in 1..10_000 do 
        let g = Array.map (translate i) f

        //let manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

        let euclideanDistance (x1, y1) (x2, y2) =
            sqrt (float ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)))

        let mutable numClose = 0
        for i in 0 .. g.Length - 2 do
            for j in i + 1 .. g.Length - 1 do
                let d = euclideanDistance g[i] g[j]
                if d > 0 && d < 3 then 
                    numClose <- numClose + 1 
            
        printfn "i=%d" i
        if numClose > 800 then 
            writer.WriteLine "################################################"
            writer.WriteLine $"i={i}"

            let grid = Array.init h (fun _ -> Array.create w '.')
    
            for (x, y) in g do
                grid.[y].[x] <- '1'
            
            for row in grid do
                writer.WriteLine(String row)
            writer.Flush()

    writer.Flush()
    writer.Close()
//pt1()
pt2()