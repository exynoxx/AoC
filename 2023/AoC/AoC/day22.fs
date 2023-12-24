module day22

open System.IO
open System.Collections.Generic

open utils

type point = { x: int; y: int; z: int }
type rect = { a: point; b: point }

let toDict (tupls:('a*'b) seq) = 
    tupls |> Seq.map (fun (k,v)-> KeyValuePair.Create(k,v)) |> Dictionary
let mapDict (dict:Dictionary<'a,'b>) (fkey: 'a -> 'g) (fval: 'b -> 'h) = 
    dict |> Seq.map (fun kv -> KeyValuePair.Create(fkey kv.Key, fval kv.Value)) |> Dictionary


let parse (s:string) = 
    let [|a;b|] = s.Split('~')
    let [|x;y;z|] = a.Split(',')
    let [|f;g;h|] = b.Split(',')
    {a = {x=int x;y=int y;z=int z};b = {x=int f ;y= int g;z= int h}}

let parseAll () = 
    File.ReadAllLines("/home/nicholas/Documents/git/AoC/2023/AoC/AoC/day22.txt") |> Seq.map parse |> List.ofSeq

//type TreeNode = { overlap: rect list; left: TreeNode option; right: TreeNode option }

let overlap rect x = 
    let xaxb = ((max rect.a.x x.a.x), (min rect.b.x x.b.x))
    let yayb = ((max rect.a.y x.a.y), (min rect.b.y x.b.y))
    (xaxb,yayb)

let doesOverlap ((xa,xb),(ya,yb)) = xa <= xb && ya <= yb

//horizontal surfaces
let hcover (hsurfaceset: Dictionary<rect, rect>) intersector = 
    if intersector.a.z = intersector.b.z then
        for x in hsurfaceset.Keys do
            let ((xa,xb),(ya,yb)) = overlap intersector hsurfaceset[x]
            if (doesOverlap ((xa,xb),(ya,yb))) && intersector.a.z > x.a.z then
                hsurfaceset[x] <- {a={x=xa;y=ya;z=x.a.z};b={x=xb;y=yb;z=0}}
    else
        ()

    hsurfaceset.Add ((intersector,intersector))

//Vertical surfaces
let vcover (vsurfaceset:HashSet<rect>) intersector = 
    if intersector.a.z = intersector.b.z then
        for x in vsurfaceset do
            let xoverlap = intersector.a.x <= x.a.x && x.a.x <= intersector.b.x
            let yoverlap = intersector.a.y <= x.a.y && x.a.y <= intersector.b.y
            if (xoverlap || yoverlap)  && intersector.a.z > x.a.z then
                vsurfaceset.Remove x |> ignore
    else
        //vertical on vertical
        for x in vsurfaceset do
            if intersector.a.x = x.a.x && intersector.a.y = x.a.y && intersector.a.z > x.a.z then
                vsurfaceset.Remove x |> ignore

let fall (supports: Dictionary<rect, rect list>) (newToOld: Dictionary<rect,rect>) (hsurfaceset: Dictionary<rect, rect>) (vsurfaceset:HashSet<rect>) r = 
    
    let surfaces_h = 
        hsurfaceset.Keys
        |> Seq.filter (fun key -> doesOverlap (overlap r hsurfaceset[key])) 

    let surfaces_v = 
        vsurfaceset 
        |> Seq.filter (fun x -> r.a.x = x.a.x && r.a.y = x.a.y)

    let surfaces = Seq.append surfaces_h surfaces_v |> List.ofSeq

    let top = surfaces |> List.map _.a.z |> listmax 0
    let same = surfaces |> Seq.filter (fun rect -> rect.a.z = top) |> List.ofSeq
    for block in same do
        supports[newToOld[block]] <- r::supports[newToOld[block]]
 
    match r.a.z = r.b.z with
    | true -> 
        //horizontal 
        let newr = {a={x=r.a.x;y=r.a.y;z=top+1};b={x=r.b.x;y=r.b.y;z=top+1}}
        hcover hsurfaceset newr |> ignore
        newToOld[newr] <- r 
        newr
        
    | false -> 
        //vertical
        let h = r.a.z - r.b.z
        let newr = {a={x=r.a.x;y=r.a.y;z=top+1+h};b={x=r.b.x;y=r.b.y;z=top+1+h}}
        vcover vsurfaceset newr |> ignore
        newToOld[newr] <- r 
        newr

let printrect rect = printfn $"{rect.a.x} {rect.a.y} {rect.a.z} -> {rect.b.x} {rect.b.y} {rect.b.z}"
let pt1() = 
    let rects = parseAll () |> List.sortBy (fun r -> r.b.z) //maybe fatal to sort
    
    let hsurfaceset = new Dictionary<rect, rect>()
    let vsurfaceset = new HashSet<rect>()
    let newToOld = new Dictionary<rect,rect>()
    let supports = rects |> dict id (fun _ -> [])

    let settled = 
        rects 
        |> Seq.map (fun r -> (r, fall supports newToOld hsurfaceset vsurfaceset r)) 
        |> toDict

    
    
    printfn $"0"


pt1()