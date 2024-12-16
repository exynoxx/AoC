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

let pt1 () = 

    let index slots =
        let accumulate (idx, acc) = 
            function 
            | File(_, id, size) -> (idx + size, File(idx, id, size) :: acc)
            | Space(_, size) -> (idx + size, Space(idx, size) :: acc)

        slots |> List.fold accumulate (0, []) |> snd |> List.rev

    //Triangular numbers
    let squarenum n = n*(n+1L) / 2L

    let rec block_sum = //sum for block (a..b) = id*(a+1+..+b) = id * (sum a to n) = id * (sum 1 to n - sum 1 to a-1) => apply triange numbers
        function
        | File (i, id, size) -> 
            let n = i+size-1L
            id * (squarenum n - squarenum (i-1L))

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
    printfn "pt1 %i" result

pt1()