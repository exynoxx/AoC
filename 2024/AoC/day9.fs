module day9

open System.IO

let disk = File.ReadAllText("data/day9.txt").ToCharArray() |> Array.map (fun c -> int c - int '0')

let squarenum n = n*(n+1) / 2

let sum (s:int) (n:int) : int = 
    squarenum n - squarenum (s-1)
    
//sum_i=a^n(i*n) = n * sum_i=1^n(i) - sum_i=1^a-1(i)

let rec checksum (t,s) (tar,src) : int = 
    match () with 
    | _ when s < t -> 1
    //file
    | _ when t%2 = 0 && disk[t] = 0 -> checksum (t+1, s) (tar,src)
    | _ when t%2 = 0 -> disk[t] * (sum t disk[t]) * checksum (t+1, s) (tar,src)
    //fill freespace
    | _ when disk[t] = 0 -> checksum (t+1, s) (tar,src)
    | _ when tar = src -> disk[t] * (sum t tar) * checksum (t+1, s-2) (disk[t+2], disk[s-2])
    | _ when tar < src -> disk[s] * (sum t tar) * checksum (t+1, s) (disk[t+2],src-tar)
    | _ when tar > src -> disk[s] * (sum t src) * checksum (t, s-2) (src-tar,disk[s-2])
    
if disk.Length % 2 = 0 then 
    failwith ""
let result = checksum (0, disk.Length-1) (disk[0],disk[disk.Length-1])
printfn "%i" result