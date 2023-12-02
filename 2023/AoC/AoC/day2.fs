module day2 
    
open System.IO
open System.Collections.Generic
open System

type Entry = int * string
type Bag = Entry seq
type Game = Bag seq

let parseLine (s:string) = 
    let stringToEntry (str:string) = let [| num; word |] = str.Trim().Split(' ') in int num, word
    let stringToBag (str:string) = str.Split(',') |> Seq.map stringToEntry
    let stringToGame (str:string) = str.Split(';') |> Seq.map stringToBag
    
    let [| rawId; content |] = s.Split(':')
    let id = rawId.Split(' ') |> Seq.last |> int
    let game = stringToGame content
    (id,game)

let pt1() = 
    let EntryLessThan = function
        | (x, "red") -> x <= 12
        | (x, "green") -> x <= 13
        | (x, "blue") -> x <= 14

    let goodBag bag = Seq.forall EntryLessThan bag
    let goodGame (_, game:Game) = Seq.forall goodBag game

    let lines = File.ReadAllLines("day2.txt")
    let games = lines |> Seq.map parseLine
    let filtered = games |> Seq.filter goodGame
    let sum = filtered |> Seq.sumBy fst
    printfn $"{sum}"


let pt2 () =
    let vectorize = function
        | (x, "red") -> (x, 0, 0) 
        | (x, "green") -> (0, x, 0)
        | (x, "blue") -> (0, 0, x)

    let bagToVector bag =
        bag 
        |> Seq.map vectorize
        |> Seq.reduce (fun (a,b,c) (x,y,z) -> (a+x, b+y, c+z))

    let combine (a,b,c) (x,y,z) = (max a x, max b y, max c z)
    let powerOfGame (_, game) =
        let (x,y,z) = game |> Seq.map bagToVector |> Seq.reduce combine
        x*y*z

    let sum = File.ReadAllLines("day2.txt")
            |> Seq.map parseLine 
            |> Seq.map powerOfGame 
            |> Seq.sum

    printfn $"{sum}"   

pt1()
pt2()