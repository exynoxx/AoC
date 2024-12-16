module day9

open System.IO
open System.Collections.Generic

type Slot = 
    | File of int*int*int //index, id, size
    | Space of int*int//index, size



let rec indexAndDiscard idx = 
    function
    | [] -> []
    | File(_,_,size)::xs when size = 0 -> (indexAndDiscard idx xs)
    | Space(_,size)::xs when size = 0 -> (indexAndDiscard idx xs)
    | File (_,id,size)::xs -> File(idx, id, size)::(indexAndDiscard (idx+size) xs)
    | Space (_,size)::xs -> Space (idx,size)::(indexAndDiscard (idx+size) xs )

let rec sorted (x::xs:Slot list) (y::ys:Slot list) idx =
    match x with
    | File (_,id,size) -> 
        match y with 
        | File (_,yid,_) when id > yid -> []
        | File (_,yid,_) when id = yid -> [y]
        | _ -> x :: sorted xs (y::ys) (idx+size)
    | Space(_, space_size) -> 
        match y with
        | Space _ -> sorted (x::xs) (ys) idx
        | File (_, file_id, file_size) when space_size = file_size -> 
            //fit
            File (idx, file_id, space_size) :: sorted xs ys (idx+file_size)

        | File (_, file_id, file_size) when space_size > file_size  -> 
            //put file in space
            let space_remains = Space (idx+file_size, space_size - file_size)
            File(idx, file_id, file_size) :: sorted (space_remains :: xs) ys (idx+file_size)

        | File (file_index, file_id, file_size) when space_size < file_size  -> 
            //fill space up & split file
            let pt1 = File (idx, file_id, space_size)
            let pt2 = File (file_index, file_id, file_size - space_size)
            pt1 :: sorted xs (pt2::ys) (idx+space_size)

//Triangular numbers
let squarenum n = n*(n+1) / 2

let rec checksum = 
    function
    | [] -> 0
    | Space _ :: xs -> failwith ""
    | File (i, id, size) :: xs -> 
        let n = i+size-1
        let sumn = squarenum n
        let sumi = squarenum (i-1)
        let blocksum = id * (sumn - sumi)
        blocksum + checksum xs

let slots = File.ReadAllText("data/day9.txt").ToCharArray() 
            |> Array.map (fun c -> int c - int '0')
            |> Array.mapi(fun i e -> if i % 2 = 0 then File (i, (i/2), e) else Space (i,e)) 
            |> List.ofArray
            |> indexAndDiscard 0

let slots_sorted = sorted slots (List.rev slots) 0
let result = checksum slots_sorted
printfn "%i" result





(*let sort (arr:Slot list) offset = 
    let rev = List.rev arr

    let mutable idx = offset
    let mutable i = 0
    let mutable last_file = 1000000
    let mutable j = arr.Length - 1

    while idx < last_file do
        match arr[i] with 
            | File _ -> i <- i + 1
            | Space(_,space_size) -> 
                match arr[j] with
                | Space (_,space_size) -> j <- j - 1
                | File (_, file_id, file_size) when space_size = file_size -> 
                    yield File (idx, file_id, space_size)
                    idx <- idx + size
                    i <- i + 1
                    j <- j - 1

                | File (file_index, file_id, file_size) when space_size > file_size  -> 
                    //put file in space
                    let space_remains = Space (space_index, space_size - file_size)
                    (sum space_index file_size file_id) + checksum (space_remains::xs) ys


                | File (file_index, file_id, file_size) when space_size < file_size  -> 
                    //fill space up & split file
                    let file_remains = File (file_index, file_id, file_size - space_size)
                    (sum space_index space_size file_id) + checksum xs (file_remains::ys)

    seq {
        while i < j do 
            match arr[i] with 
            | File (_,id,size) -> 
                yield File (idx,id,size)
                idx <- idx + size
                i <- i + 1

            | Space(_,space_size) -> 
                match arr[j] with
                | File (_, file_id, file_size) when space_size = file_size -> 
                    yield File (idx, file_id, space_size)
                    idx <- idx + size
                    i <- i + 1
                    j <- j + 1

                | File (file_index, file_id, file_size) when space_size > file_size  -> 
                    //put file in space
                    let space_remains = Space (space_index, space_size - file_size)
                    (sum space_index file_size file_id) + checksum (space_remains::xs) ys


                | File (file_index, file_id, file_size) when space_size < file_size  -> 
                    //fill space up & split file
                    let file_remains = File (file_index, file_id, file_size - space_size)
                    (sum space_index space_size file_id) + checksum xs (file_remains::ys)

                

        for slot in arr do 
            
    }
   

let visited = HashSet<int>()
*)
(*let rec checksum (slots : Slot list) (rev:Slot list) : int = 
    match (slots, rev) with
    | ([],[]) -> 1
    | (x::xs, y::ys) ->   
        if index x >= index y then 
            1
        else
            match x with
            | File (index, ID, size) ->  (sum index size ID) + checksum xs rev
            | Space (space_index, space_size) -> 
                match y with
                | Space _ -> checksum slots ys
                | File (file_index, file_id, file_size) when space_size = file_size -> 
                    (sum space_index space_size file_id) + checksum xs ys

                | File (file_index, file_id, file_size) when space_size < file_size  -> 
                    //fill space up & split file
                    let file_remains = File (file_index, file_id, file_size - space_size)
                    (sum space_index space_size file_id) + checksum xs (file_remains::ys)

                | File (file_index, file_id, file_size) when space_size > file_size  -> 
                    //put file in space
                    let space_remains = Space (space_index, space_size - file_size)
                    (sum space_index file_size file_id) + checksum (space_remains::xs) ys

                | _ -> failwith ""*)

    (*match () with 
    | _ when s < t -> 1
    //file
    | _ when t%2 = 0 && size[t] = 0 -> checksum (t+1, s) (tar,src)
    | _ when t%2 = 0 -> (sum t size[t] (file_id t)) * checksum (t+1, s) (tar,src)
    //fill freespace
    | _ when size[t] = 0 -> checksum (t+1, s) (tar,src)
    | _ when tar = src -> *)(*size[t] * *)(*(sum t tar (file_id s)) * checksum (t+1, s-2) (size[t+2], size[s-2])
    | _ when tar < src -> *)(*size[s] * *)(*(sum t tar (file_id s)) * checksum (t+1, s) (size[t+2],src-tar)
    | _ when tar > src -> *)(*size[s] * *)(*(sum t src (file_id s)) * checksum (t, s-2) (src-tar,size[s-2])*)
    
