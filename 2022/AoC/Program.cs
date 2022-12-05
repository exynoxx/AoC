using System.Collections;
using AoC;


static class day5
{

    public static void pt1()
    {
        var towers = new List<Stack<char>>();
        
        var fileLines = Utils.GetFileLines("input5.txt");
        foreach (var fileLine in fileLines.Take(10))
        {
            if(fileLine=="xx") break;

            var elements = fileLine.ToCharArray()
                .Skip(1)
                .ToList();

            towers.Add(elements.ToStack());
        }
        
        foreach (var fileLine in fileLines.Skip(10))
        {
            var l = fileLine.Split(" ");
            int num = l[1].ToInt();
            int from = l[3].ToInt()-1;
            int to = l[5].ToInt()-1;

            for (int i = 0; i < num; i++)
            {
                towers[to].Push(towers[from].Pop());
            }
        }

        towers.Select(x => x.Pop()).Join("").Print();
    }

    public static void pt2()
    {
        var towers = new List<Stack<char>>();
        
        var fileLines = Utils.GetFileLines("input5.txt");
        foreach (var fileLine in fileLines.Take(10))
        {
            if(fileLine=="xx") break;

            var elements = fileLine.ToCharArray()
                .Skip(1)
                .ToList();

            towers.Add(elements.ToStack());
        }
        
        foreach (var fileLine in fileLines.Skip(10))
        {
            var l = fileLine.Split(" ");
            int num = l[1].ToInt();
            int from = l[3].ToInt()-1;
            int to = l[5].ToInt()-1;

            var tmp = new Stack<char>();
            for (int i = 0; i < num; i++)
            {
                tmp.Push(towers[from].Pop());
            }

            foreach (var e in tmp)
            {
                towers[to].Push(e);
            }
        }

        towers.Select(x => x.Pop()).Join("").Print();
    }

    public static void Main(string[] args)
    {
        day5.pt2();
    }
}