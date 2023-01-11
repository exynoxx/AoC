namespace AoC

open System
open System.Collections.Generic
open System.IO


module Day18 =
    (*let allDirections =
        let rec f (l:int list) =
            match l with
            | x when x.Length = 3 -> [l]
            | _ -> f (-1::l) @ f (0::l) @ f(1::l)
        f []*)
(*    let cartesianProduct l = l |> List.fold (
        fun acc -> List.collect (
            fun x -> List.map (
                fun y -> x :: y) acc) ) [[]]*)
    
    type Coordinate = XYZ of int*int*int

    
    //let ToTuple (l:int list) = XYZ (List.head l, List.head (List.tail l), List.head (List.tail (List.tail l)))

    let allDirections = [XYZ (1,0,0);XYZ(-1,0,0);XYZ(0,1,0);XYZ(0,-1,0);XYZ(0,0,-1);XYZ(0,0,1)]

        
    
    //let touch = Dictionary<Coordinate, Coordinate Set>()
    
    let inline (++) a b = match (a,b) with
        | XYZ (a,b,c) , XYZ (x,y,z) -> XYZ (a+x,b+y,c+z)
       
    let pt1() =
        let graph =
            File.ReadLines("../../../input18.txt")
            |> Seq.map (fun l -> l.Split ',')
            |> Seq.map (Array.map int)
            |> Seq.map (fun l -> XYZ (l[0],l[1],l[2]))
            |> Set
            
        
        let blockedSides xyz = 
            allDirections
            |> Seq.filter (fun dxyz -> graph.Contains (xyz++dxyz))
            |> Seq.length
            
        let blocked = graph |> Seq.map blockedSides |> Seq.sum
        let exposed = 6*graph.Count - blocked
        printfn "%d" exposed
        