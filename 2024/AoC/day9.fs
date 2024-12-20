module day9

open System.IO
open System.Collections.Generic
open DataStructures
open Utils

type Slot = 
    | File of int64*int64*int64 //index, id, size
    | Space of int64*int64//index, size

let not_empty = 
    function
    | File (_, _, size) when size > 0 -> true
    | Space (_, size) when size > 0 -> true
    | _ -> false

let index slots =
    let accumulate (idx, acc) = 
        function 
        | File(_, id, size) -> (idx + size, File(idx, id, size) :: acc)
        | Space(_, size) -> (idx + size, Space(idx, size) :: acc)

    slots |> List.fold accumulate (0, []) |> snd |> List.rev

//Triangular numbers
let inline squarenum n = n*(n+1L) / 2L

let file_sum (File (i, id, size)) = //sum for block (a..b) = id*(a+1+..+b) = id * (sum a to n) = id * (sum 1 to n - sum 1 to a-1) => apply triange numbers
    let n = i+size-1L
    id * (squarenum n - squarenum (i-1L))

let slots = File.ReadAllText("data/day9.txt").ToCharArray() 
            |> Array.map (fun c -> int c - int '0')
            |> Array.mapi(fun i e -> if i % 2 = 0 then File (i, ((int64)i/2L), e) else Space (i,e)) 
            |> Array.where not_empty
            |> List.ofArray
            |> index

let pt1 () = 
    let rec checksum (x::xs:Slot list) (y::ys:Slot list) sum =
        match x with
        | File (_,id,_) -> 
            match y with 
            | File (_,yid,_) when id > yid -> sum
            | File (_,yid,_) when id = yid -> sum + file_sum y
            | _ -> checksum xs (y::ys) (sum+file_sum x)

        | Space(sidx, space_size) -> 
            match y with
            | Space _ -> checksum (x::xs) (ys) sum 
            | File (_, file_id, file_size) when space_size = file_size -> 
                //fit
                let file = File (sidx, file_id, space_size)
                checksum xs ys (sum+file_sum file)

            | File (_, file_id, file_size) when space_size > file_size  -> 
                //put file in space
                let space_remains = Space (sidx+file_size, space_size - file_size)
                let file = File(sidx, file_id, file_size)
                checksum (space_remains :: xs) ys (sum+file_sum file)

            | File (file_index, file_id, file_size) when space_size < file_size  -> 
                //fill space up & split file
                let pt1 = File (sidx, file_id, space_size)
                let pt2 = File (file_index, file_id, file_size - space_size)
                checksum xs (pt2::ys) (sum+file_sum pt1)

    let result = checksum slots (List.rev slots) 0
    printfn "pt1 %i" result

type Space_malloc () = 
    let heap_key =
        function
        | File (i, _, _) -> i
        | Space (i, _) -> i
    let space_buckets = [| for _ in 0 .. 9 do MinHeap<Slot,int64>(heap_key) |]
    
    let Insert_slot = 
        function
        | Space(i,s) -> space_buckets[(int)s].Insert(Space(i,s))
        | File _ -> ()

    member _.Import (list: Slot list) = 
        for slot in list do
            Insert_slot slot

    member _.Find_space (size:int64) = 
        let (_, i) =
            [size .. 9]
            |> List.filter (fun i -> space_buckets[(int)i].Any())
            |> List.map (fun i -> (space_buckets[(int)i].Peek(), i))
            |> MinByOrDefault (Space(0,0),-1) fst
    
        if i > 0 then 
            Some (space_buckets[(int)i].RemoveMin())
        else 
            None

    member _.Reinsert = Insert_slot

let pt2() = 

    let malloc = Space_malloc()
    malloc.Import slots

    let rec checksum (l:Slot list) sum =
        match l with
        | [] -> sum
        | x::xs -> 
            match x with
            | File (fidx,id,fsize) -> 
                match malloc.Find_space fsize with
                | Some (Space(space_idx,ssize)) when ssize = fsize && fidx > space_idx -> 
                    let new_file = File (space_idx, id, fsize)
                    malloc.Reinsert (Space (fidx, fsize))
                    checksum xs (sum + file_sum new_file)
                | Some (Space(sidx,ssize)) when ssize > fsize && fidx > sidx -> 
                    malloc.Reinsert (Space (sidx+fsize, ssize-fsize)) //remains
                    malloc.Reinsert (Space (fidx, fsize)) //original file location
                    let new_file = File (sidx, id, fsize)
                    checksum xs (sum + file_sum new_file)
                | _ -> //None or space to right of file
                    checksum xs (sum + file_sum x)
            | Space(_, _) -> checksum xs sum

    let result = checksum (List.rev slots) 0
    printfn "pt2 %i" result


pt1()
pt2()