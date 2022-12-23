﻿using AoC;

static class day12
{

    public record Pos(int x, int y);

    public static IEnumerable<Pos> Adj(this Dictionary<Pos, char> graph, Pos u)
    {
        var dx = new[] { -1, 0, 1, 0 };
        var dy = new[] { 0, 1, 0, -1 };

        for (var i = 0; i < 4; i++)
        {
            var v = new Pos(u.x + dx[i], u.y + dy[i]);
            if(graph.ContainsKey(v)) 
                yield return v;
        }
    }

    public static (int, HashSet<Pos> visited) Dijkstra(Dictionary<Pos, char> graph, Pos S)
    {
        var dist = new Dictionary<Pos, int>();
        var visited = new HashSet<Pos>();
        var Q = new PriorityQueue<Pos,int>();
        dist[S] = 0;
        graph[S] = 'a';
        Q.Enqueue(S,-'a');
        Pos E = null;

        char max = 'a';
        while (Q.TryDequeue(out var u, out _))
        {
            if (visited.Contains(u)) continue;
            visited.Add(u);
            var uh = graph[u];

            if (uh == 'E')
                E = u;
            if(uh > max) max = uh;
            if(max-uh >= 2) break;
            uh.Print();
            
            foreach (var v in graph.Adj(u))
            {
                var vh = graph[v];
                if (vh != 'E' && vh-uh <= 1 || uh == 'z' && vh == 'E')
                {
                    dist[v] = Math.Min(dist.GetValueOrDefault(v,char.MaxValue), dist[u] + 1);
                    Q.Enqueue(v,-vh);
                }
            }
        }

        return (dist[E],visited);
    }
    
    public static void pt1()
    {
        var S = new Pos(-1,-1);

        var file = Utils
            .GetFileLines("input12.txt")
            .ToList();

        var n = file.Count;
        var m = file.First().Length;
        
        var g = file
            .SelectMany((x, i) => x.ToCharArray().Select((y, j) => {
                if (y == 'S')
                {
                    S = new Pos(i, j);
                }
                return new {Pos = new Pos(i, j),y};
            }))
            .ToDictionary(x=>x.Pos, x=>x.y);

        var (res, visited) = Dijkstra(g, S);
        res.Print();

        var grid = new char[n, m];
        foreach (var p in g)
        {
            grid[p.Key.x, p.Key.y] = p.Value;
        }
        foreach (var p in visited)
        {
            grid[p.x, p.y] = '.';
        }

        var lines = new List<string>();
        for (int i = 0; i < n; i++)
        {
            var s = "";
            for (int j = 0; j < m; j++) s+= grid[i, j];
            lines.Add(s);
        }
        Utils.WriteLines("output12.txt", lines);

    }
}