module day9

open System.IO
open System.Collections.Generic

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

    slots
    |> List.fold accumulate (0, [])
    |> snd
    |> List.rev

//Triangular numbers
let squarenum n = n*(n+1L) / 2L

let rec block_sum = 
    function
    | Space _ -> failwith ""
    | File (i, id, size) -> 
        let n = i+size-1L
        let sumn = squarenum n
        let sumi = squarenum (i-1L)
        id * (sumn - sumi)

let rec checksum (x::xs:Slot list) (y::ys:Slot list) idx sum =
    match x with
    | File (_,id,size) -> 
        match y with 
        | File (_,yid,_) when id > yid -> sum
        | File (_,yid,_) when id = yid -> sum + block_sum y
        | _ -> checksum xs (y::ys) (idx+size) (sum+block_sum x)

    | Space(_, space_size) -> 
        match y with
        | Space _ -> checksum (x::xs) (ys) idx sum 
        | File (_, file_id, file_size) when space_size = file_size -> 
            //fit
            let file = File (idx, file_id, space_size)
            checksum xs ys (idx+file_size) (sum+block_sum file)

        | File (_, file_id, file_size) when space_size > file_size  -> 
            //put file in space
            let space_remains = Space (idx+file_size, space_size - file_size)
            let file = File(idx, file_id, file_size)
            checksum (space_remains :: xs) ys (idx+file_size) (sum+block_sum file)

        | File (file_index, file_id, file_size) when space_size < file_size  -> 
            //fill space up & split file
            let pt1 = File (idx, file_id, space_size)
            let pt2 = File (file_index, file_id, file_size - space_size)
            checksum xs (pt2::ys) (idx+space_size) (sum+block_sum pt1)

let slots = File.ReadAllText("data/day9.txt").ToCharArray() 
            |> Array.map (fun c -> int c - int '0')
            |> Array.mapi(fun i e -> if i % 2 = 0 then File (i, ((int64)i/2L), e) else Space (i,e)) 
            |> Array.where not_empty
            |> List.ofArray
            |> index

let result = checksum slots (List.rev slots) 0 0
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
    
