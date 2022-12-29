using System.Linq;
using System.Text;
using AoC;

static class day14Backup
{

    public static (int, int) Source = (500, 0);


    public static (int x,int y)? binarySeach(ref List<(int u, int v)> array, int l, int r, int x)
    {
        if (!array.Any()) return null;
        if (l >= r) return null;

        int mid = (l + r) / 2;    
        
        // We found equal key
        if (array[mid].u <= x && x <= array[mid].v)
            return array[mid];
 
        if (x > array[mid].v)
        {
            return binarySeach(ref array, mid+1,r,x);
        }
        else 
        {
            return binarySeach(ref array, l,mid-1,x);
        }
    }
    
    public static (int x, int y) Fall(ref Dictionary<int, List<(int, int)>> horizontal, ref Dictionary<int, List<(int, int)>> vertical, ref Dictionary<int, Dictionary<int,bool>> sand, (int, int) s)
    {
        var x = s.Item1;
        var y = s.Item2;
        List<(int,int)> xs;
        (int, int)? bar;
        

        while (true)
        {
            if (sand[x][y + 1])
            {
                bar = (x, y);
                break;
            }

            xs = horizontal[y];
            bar = binarySeach(ref xs, 0, xs.Count - 1, x);
            if (bar != null)
            {
                break;
            }
            y++;
        }
        if (sand[x - 1][y + 1]) return (x - 1, y + 1);
        if (sand[x + 1][y + 1]) return (x + 1, y + 1);
        return (x,y);
    }
    
    public static void pt1()
    {
        var horizontal = new Dictionary<int, List<(int, int)>>();
        var vertical = new Dictionary<int, List<(int, int)>>();
        var sand = new Dictionary<int, Dictionary<int,bool>>();
        var file = Utils.GetFileLines("input14.txt").ToList();
        foreach (var line in file)
        {
            var coords = line.Split(" -> ").Select(s =>
            {
                var coord = s.Split(",");
                return (coord[0].ToInt(), coord[1].ToInt());
            });
            var last = coords.First();
            foreach (var (x,y) in coords.Skip(1))
            {
                if (last.Item1 == x)
                {
                    vertical.GetOrCreate(x).Add((last.Item2,y));
                }
                else
                {
                    horizontal.GetOrCreate(y).Add((last.Item1,x));
                }

                last = (x, y);
            }
        }
        
        //fixup and sort
        foreach (var list in horizontal.Values)
        {
            for (int i = 0; i < list.Count; i++)
                if (list[i].Item1 > list[i].Item2)
                    list[i] = (list[i].Item2, list[i].Item1);
            list.Sort();
        }
        foreach (var list in vertical.Values)
        {
            for (int i = 0; i < list.Count; i++)
                if (list[i].Item1 > list[i].Item2)
                    list[i] = (list[i].Item2, list[i].Item1);
            list.Sort();
        }
        

        var lastDst = Source;
        var count = -1;
        while (true)
        {
            var dst = Fall(ref horizontal, ref vertical, ref sand, lastDst);
            if (dst.y == -1) break;
            lastDst = (dst.x, dst.y-1);
            sand[dst.x][dst.y] = true;
            count++;
        }
        Console.WriteLine(count);
    }

   
}


static class Day14
{

    public static (int x, int y)? Fall(ref bool[,] grid, (int x, int y) s)
    {
        var (x, y) = s;
        while (!grid[x, y+1] && y < 600) y++;
        if (y == 600) return null;
        if (!grid[x - 1, y + 1]) return (x - 1, y + 1);
        if (!grid[x + 1, y + 1]) return (x + 1, y + 1);
        return (x, y);
    }

    public static void pt1()
    {
        var file = Utils.GetFileLines("input14.txt").ToList();
        var grid = new bool[600, 600];
        foreach (var line in file)
        {
            var coords = line.Split(" -> ").Select(s =>
            {
                var coord = s.Split(",");
                return (coord[0].ToInt(), coord[1].ToInt());
            });
            var last = coords.First();
            foreach (var (x,y) in coords.Skip(1))
            {
                if (last.Item1 == x)
                    for (int dy = 0; dy < y - last.Item2; dy++)
                        grid[x, y + dy] = true;
                else
                    for (int dx = 0; dx < x - last.Item1; dx++)
                        grid[x+dx, y] = true;
                last = (x, y);
            }
        }
    }
    
    public static void Main(string[] args)
    {
        pt1();
    }
}
