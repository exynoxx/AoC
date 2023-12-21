module day12

open System.Collections.Generic
open System.IO

let rec arrangments (row:char array) (nums:int array) i hashs j (acc:string) = 
    if acc.StartsWith ".###.##." then
        ()
    let dotCase () = 
        match hashs with 
        | 0 -> memoArrangments row nums (i+1) 0 j (acc + ".")
        | x when x = nums[j] -> memoArrangments row nums (i+1) 0 (j+1) (acc + ".")
        | _ -> 0 //maybe rm

    let hashCase () = memoArrangments row nums (i+1) (hashs+1) j (acc + "#")
                       
    if i >= row.Length then 
        //finish block
        if j = nums.Length - 1 && hashs = nums[j] then
            1
        elif j = nums.Length then 
            1 
        else 
            0
    else
        match row[i] with
        | '#' when j = nums.Length -> 0
        | _ when j = nums.Length -> dotCase()
        | '.' -> dotCase()
        | '#' -> hashCase()
        | '?' -> 
            match hashs with
            | x when x = nums[j] -> dotCase()
            | x when x > nums[j] -> 0
            | _ -> 
                let l = hashCase() 
                let r = dotCase()
                l+r

and memoArrangments (row:char array) (nums:int array) i hashs j acc =
    arrangments row nums i hashs j acc

let niceArrangments (row, nums) = memoArrangments row nums 0 0 0 ""

let parse (s:string) = 
    let [|left;right|] = s.Split (' ')
    let nums = right.Split(',') |> Array.map int
    (left.ToCharArray(),nums)

let pt1() = 
    let list = File.ReadAllLines("day12.txt") |> Seq.map parse |> Seq.map niceArrangments |> List.ofSeq
    let sum = list |> List.sum
    let out = list |> List.map string |> String.concat ","
    printfn $"{out} {sum}"

pt1()

(*let list = ["..?#??#???#?????. 5,6"] |> Seq.map parse |> Seq.map niceArrangments |> List.ofSeq
let sum = list |> List.sum
printfn $"{sum}"*)