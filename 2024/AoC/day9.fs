module day9

open System.IO
open System.Collections.Generic


type Slot = 
    | File of int*int*int //index, id, size
    | Space of int*int//index, size

let non_empty = 
    function
    | File(_,_,size) -> size > 0
    | Space(_,size)  -> size > 0

let slots = File.ReadAllText("data/day9.txt").ToCharArray() 
            |> Array.map (fun c -> int c - int '0')
            |> Array.mapi(fun i e -> if i % 2 = 0 then File (i, (i/2), e) else Space (i,e)) 
            |> Array.filter non_empty
            |> List.ofArray

let squarenum n = n*(n+1) / 2

let sum (s:int) (n:int) (id:int) : int = 
    //squarenum n - squarenum (s-1)
    id * (seq {s .. s+n} |> Seq.sum)
    
let file_id idx = idx / 2
let index = function
            | File (index, _, _) -> index
            | Space (index, _) -> index

//sum_i=a^n(i*n) = n * sum_i=1^n(i) - sum_i=1^a-1(i)

let visited = HashSet<int>()

let rec checksum (slots : Slot list) (rev:Slot list) : int = 
    match (slots, rev) with
    | ([],[]) -> 1
    | (x::xs, y::ys) ->   
        if index x > index y then 
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

                | _ -> failwith ""

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
    
let result = checksum (slots) (List.rev slots)
printfn "%i" result