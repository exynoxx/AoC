module day12

open System.Collections.Generic
open System.IO

let rec arrangments (dict:Dictionary<int*int*int,int64>) (row:char array) (nums:int array) i hashs j = 

    let dot () = 
        match hashs with 
        | 0 -> memoArrangments dict row nums (i+1) 0 j
        | x when x = nums[j] -> memoArrangments dict row nums (i+1) 0 (j+1)
        | _ -> 0L

    let hash () = memoArrangments dict row nums (i+1) (hashs+1) j
                       
    if i >= row.Length then 
        //finish block
        if j = nums.Length - 1 && hashs = nums[j] || j = nums.Length then
            1L
        else 
            0L
    else
        match row[i] with
        | '#' when j = nums.Length -> 0L
        | _ when j = nums.Length -> dot()
        | '.' -> dot()
        | '#' -> hash()
        | '?' -> 
            match hashs with
            | x when x = nums[j] -> dot()
            | _ -> hash() + dot()

and memoArrangments dict row nums i hashs j =
    match dict.TryGetValue ((i,hashs,j)) with
    | (true, x) -> x
    | _ -> 
        let value = arrangments dict row nums i hashs j
        dict.Add ((i,hashs,j), value)
        value

let niceArrangments (row, nums) = memoArrangments (new Dictionary<int*int*int,int64>()) row nums 0 0 0

let pt1() = 
    let parse (s:string) = 
        let [|left;right|] = s.Split (' ')
        let nums = right.Split(',') |> Array.map int
        (left.ToCharArray(),nums)

    let sum = File.ReadAllLines("day12.txt") |> Seq.map parse |> Seq.map niceArrangments |> Seq.sum
    printfn $"{sum}"

let pt2() = 
    let parse (s:string) = 
        let [|left;right|] = s.Split (' ')
        let springs = left |> List.replicate 5 |> String.concat "?" |> _.ToCharArray()
        let nums = right.Split(',') |> Seq.map int |> Array.ofSeq |> Array.replicate 5 |> Array.ofSeq |> Array.collect (fun x -> x)
        (springs,nums)

    let sum = File.ReadAllLines("day12.txt") |> Seq.map parse |> Seq.map niceArrangments |> Seq.sum
    printfn $"{sum}"

pt1()
pt2()