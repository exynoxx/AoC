using System.Text.RegularExpressions;
using AoC;

public static class Day16
{
    
    public static Dictionary<string, Dictionary<string, int>> FloydWarshall(Dictionary<string, List<string>> graph)
    {
        var nodes = graph.Keys;
        var distance = new Dictionary<string, Dictionary<string, int>>();

        foreach (var x in nodes)
        {
            distance[x] = new Dictionary<string, int>();
            distance[x][x] = 0;
            foreach (var y in graph[x])
                distance[x][y] = 1;
        }
            

        foreach (var k in nodes)
        {
            foreach (var i in nodes)
            {
                foreach (var j in nodes)
                {
                    var ik = distance[i].GetValueOrDefault(k,1000);
                    var kj = distance[k].GetValueOrDefault(j,1000);
                    var ij = distance[i].GetValueOrDefault(j,1000);
                    if (ik + kj < ij)
                        distance[i][j] = ik + kj;
                }
            }
        }
        
        return distance;
    }
    
    public static int Dfs(
        Dictionary<string, List<string>> adj,
        Dictionary<string, int> flows, 
        Dictionary<string,Dictionary<string,int>> distances,
        HashSet<string> visited,
        string u,
        int t,
        int flow)
    {
        if (t <= 1) return flow;
        
        visited.Add(u);
        var max = 0;
        foreach (var v in adj.Keys)
        {
            if(visited.Contains(v)) continue;
            
            var vt = t - distances[u][v] - 1;
            var realizedFlow = vt * flows[v];
            var dfs = Dfs(adj,flows,distances,new HashSet<string>(visited),v,vt,flow+realizedFlow);
            max = Math.Max(max, dfs);
        }

        return max;
    }

    public static void pt1()
    {
        //Valve XG has flow rate=0; tunnels lead to valves CR, OH
        
        var rg = new Regex(@"Valve (\w{2}) has flow rate=(\d+); tunnel.? lead.? to valve.? (.+)$");
        var adj = new Dictionary<string, List<string>>();
        var flows = new Dictionary<string,int>();
        
        var lines = Utils.GetFileLines("input16.txt");
        foreach (var s in lines)
        {
            var groups = rg.Match(s).Groups;
            var u = groups[1].Value;
            flows[u] = groups[2].Value.ToInt();
            var neighbors = groups[3].Value.Split(", ");
            foreach (var v in neighbors)
                adj.GetOrCreate(u).Add(v);
        }
        var distances = FloydWarshall(adj);


        Dfs(adj, flows, distances, new HashSet<string>() ,"AA",30,0).Print();


    }
}