module day14

open Utils
open System.IO
open System.Collections.Generic

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

    for i in 1..300 do 
        let g = Array.map (translate i) f
        let g_unique = HashSet g

        if g.Length < g_unique.Count+20 then 
            printfn "################################################"
            printfn "unique i=%d" i

            let grid = Array.init h (fun _ -> Array.create w '.')
    
            for (x, y) in g do
                if x >= 0 && x < w && y >= 0 && y < h then
                    grid.[y].[x] <- '1'

            grid
            |> Array.iter (fun row ->
                row |> Array.map string |> String.concat "" |> printfn "%s"
            )
pt1()
//pt2()