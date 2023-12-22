module day9

open System.IO
open System.Collections.Generic

let pt1() = 
    let rec below (cache:Dictionary<int * int, int>) (nums:int array) i j =
        let key = (i,j)
        if cache.ContainsKey(key) then
            cache[key]
        else
            if i = 0 then 
                nums[j]
            else
                let r = below cache nums (i-1) j
                let l = below cache nums (i-1) (j-1)
                let result = r - l
                cache.Add(key,result)
                result
            
    let rec findZero (cache:Dictionary<int * int, int>) (nums:int array) i = 
        match below cache nums i (nums.Length - 1) with
        | 0 -> 0
        | x -> x + findZero cache nums (i+1)

    let freshFind (nums:int array) = 
        let cache = new Dictionary<int * int, int>()
        findZero cache nums 0

    let parse (s:string) = s.Split(' ') |> Array.map int
    let nums = File.ReadAllLines("day9.txt") |> Array.map parse
    
    let next = nums |> Array.map (fun num -> freshFind num)
    let sum = next |> Array.sum
    printfn $"{sum}"


let pt2() = 
    let rec below (cache:Dictionary<int * int, int>) (nums:int array) i j =
        let key = (i,j)
        if cache.ContainsKey(key) then
            cache[key]
        else
            if i = 0 then 
                nums[j]
            else
                let r = below cache nums (i-1) (j+1)
                let l = below cache nums (i-1) j
                let result = r - l
                cache.Add(key,result)
                result
            
    let rec findZero (cache:Dictionary<int * int, int>) (nums:int array) i = 
        match below cache nums i 0 with
        | 0 -> 
             match below cache nums (i+1) 0 with
             | 0 -> 0
             | x -> x - findZero cache nums (i+1)
        | x -> x - findZero cache nums (i+1)

    let freshFind (nums:int array) = 
        let cache = new Dictionary<int * int, int>()
        let r = findZero cache nums 0
        r

    let parse (s:string) = s.Split(' ') |> Array.map int
    let nums = File.ReadAllLines("day9.txt") |> Array.map parse
    
    let next = nums |> Array.map (fun num -> freshFind num)
    let sum = next |> Array.sum
    printfn $"{sum}"

pt1()
pt2()