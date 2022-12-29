
namespace AoC
open System
open System.IO

module Day14 =
    
    let rec MakeBlocks ((x1,y1), (x2,y2)) =
        let (x1,x2) = if(x2>x1) then (x1,x2) else (x2,x1)
        let (y1,y2) = if(y2>y1) then (y1,y2) else (y2,y1)
        if x1=x2 then
            seq { for dy in y1 .. y2 -> (x1,dy) }
        else
            seq { for dx in x1 .. x2 -> (dx,y1) }      
    
    let ParseLine (s:string) =  s.Split " -> " 
                                |> Seq.map (fun x -> x.Split ",")
                                |> Seq.map (fun lst -> (Int32.Parse (Seq.head lst), Int32.Parse (Seq.last lst)))
                                |> Util.Pairs
                                |> Seq.map MakeBlocks
                                |> Seq.concat
    let Parse input =
        input
        |> Seq.map ParseLine
        |> Seq.concat
        |> Set.ofSeq
        
    type typ =
        | FreeFall of (int * int)
        | Destination of (int * int)

    
    let pt1() =
        let rec IsFree (blocks:Set<int * int>) (sand:Set<int*int>) pos =
            not (blocks.Contains pos) && not (sand.Contains pos) 
        
        let rec Fall (blocks:Set<int * int>) (sand:Set<int*int>) pos =
            let GetNextLeft = (fun (a,b) -> (a-1,b+1))
            let GetNextRight = (fun (a,b) -> (a+1,b+1))
            let GetNextDown = (fun (a,b) -> (a,b+1))
            if (snd pos) >= 600 then None
            else 
                if IsFree blocks sand (GetNextDown pos) then
                    Fall blocks sand (GetNextDown pos)
                elif IsFree blocks sand (GetNextLeft pos) then
                    Fall blocks sand (GetNextLeft pos)
                elif IsFree blocks sand (GetNextRight pos) then
                    Fall blocks sand (GetNextRight pos)
                else Some pos 
    
        let Sandfall blocks =
            let rec inner blocks sand init =
                match Fall blocks sand init with
                | Some (x,y) -> 1 + inner blocks (sand.Add (x,y)) init
                | None -> 0

            inner blocks Set.empty (500,0)
        let raw = File.ReadLines("../../../input14.txt")
        let blocks = Parse raw
        let result = Sandfall blocks
        printfn $"pt1 {result}\n"
      
    let pt2() =
        let raw = File.ReadLines("../../../input14.txt")
        let blocks = Parse raw
        let maxy = (blocks |> Seq.map snd |> Seq.max) + 2
        
        let rec IsFree (blocks:Set<int * int>) (sand:Set<int*int>) pos =
            not (blocks.Contains pos) && not (sand.Contains pos) && not (snd pos = maxy)
            
        let GetNextLeft = (fun (a,b) -> (a-1,b+1))
        let GetNextRight = (fun (a,b) -> (a+1,b+1))
        let GetNextDown = (fun (a,b) -> (a,b+1))
        
        
        let rec WayPoint (blocks:Set<int * int>) (sand:Set<int*int>) pos =
            if IsFree blocks sand (GetNextDown pos) then
                WayPoint blocks sand (GetNextDown pos)
            else pos
            
        let rec Fall (blocks:Set<int * int>) (sand:Set<int*int>) pos =
            if IsFree blocks sand (GetNextDown pos) then
                FreeFall (GetNextDown pos)
            elif IsFree blocks sand (GetNextLeft pos) then
                Fall blocks sand (GetNextLeft pos) 
            elif IsFree blocks sand (GetNextRight pos) then
                Fall blocks sand (GetNextRight pos) 
            else Destination pos
            
        let rec FallCached (blocks:Set<int * int>) (sand:Set<int*int>) src : Set<int*int>  =
            let mutable sandMut = sand
            while IsFree blocks sandMut src do
                sandMut <-
                    if IsFree blocks sandMut (GetNextDown src) then
                        let waypoint = WayPoint blocks sandMut src
                        FallCached blocks sandMut waypoint
                        
                    elif IsFree blocks sandMut (GetNextLeft src) then
                        match Fall blocks sandMut (GetNextLeft src) with
                        | Destination dst -> sandMut.Add(dst)
                        | FreeFall pos -> FallCached blocks sandMut pos
                            
                    elif IsFree blocks sandMut (GetNextRight src) then
                        match Fall blocks sandMut (GetNextRight src) with
                        | Destination dst -> sandMut.Add(dst)
                        | FreeFall pos -> FallCached blocks sandMut pos
                            
                    else sandMut.Add(src)
            sandMut
            

        
        let raw = File.ReadLines("../../../input14.txt")
        let blocks = Parse raw
        let result = FallCached blocks Set.empty (500,0) |> Set.count
        printfn $"pt2 {result}\n"
        
