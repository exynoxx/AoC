namespace AoC

open System
open System.Collections.Generic
open System.Text.RegularExpressions

module Day16 =
    
    type IDictionary<'K,'V> with
        static member (+) (x : IDictionary<'K,'V>) (y : IDictionary<'K,'V>) : IDictionary<'K,'V> =
            let a : seq<KeyValuePair<'K,'V>> = x
            let b : seq<KeyValuePair<'K,'V>> = y
            Seq.append a b
            |> Seq.map (fun pair -> (pair.Key, pair.Value))
            |> dict
    
    let rx = Regex(@"Valve (\w{2}) has flow rate=(\d+); tunnel.? lead.? to valve.? (.+)$");
    
    let parseLine (s:string) =
        let captures = (rx.Match s).Groups |> Seq.map (fun x -> x.Value) |> List.ofSeq
        match captures with
        | [_;valve;flow;adj] -> (dict [valve,adj.Split(", ")], dict [valve, Int32.Parse flow])
        | _ -> raise (Exception "bad parse")

    let reduce (list:seq<IDictionary<string,string[]> * seq<IDictionary<string,int>>>) =
        let adj = list |> Seq.map fst |> Seq.reduce (+)
        let flows = list |> Seq.map snd |> Seq.reduce (+)
        adj,flows
        
    
    let parse lines = lines |> Seq.map parseLine |> reduce
    
    
    let rec DFS
        (adj:IDictionary<string, string list>)
        (flows:IDictionary<string,int>)
        (distances:IDictionary<string,IDictionary<string,int>>)
        (visited:Set<string>) u t flow : int =
        if t <= 1 then
            flow
        else
            let newvisit = visited.Add(u)
            let unvisited = (adj.Keys |> Seq.filter (fun v -> not (newvisit.Contains v)))
            let GetFlow v =
                let vt = t - distances[u][v] - 1
                let realizedFLow = vt * flows[v]
                DFS adj flows distances newvisit v vt (flow+realizedFLow)
            
            let max = unvisited
                      |> Seq.map GetFlow
                      |> Seq.max
            max
            
                
                
                
            
    
    
                