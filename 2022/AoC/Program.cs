using AoC;

static class day4
{
    static bool FullyContained(int a, int b, int x, int y) => a <= x && b >= y;

    public static void pt1()
    {
        
        Utils
            .GetFileLines("input4.txt")
            .Map(x=>x.Split(",").SelectMany(y=>y.Split("-")))
            .Map(Utils.ToIntList)
            .Count(x => FullyContained(x[0],x[1],x[2],x[3])||FullyContained(x[2],x[3],x[0],x[1]))
            .Print();
    }
    
    public static void pt2()
    {
        var PartialOverlap = (int a, int b, int x, int y) => 
            a <= x && x <= b ||
            x <= a && a <= y ||
            FullyContained(a,b,x,y)||
            FullyContained(x,y,a,b);

        Utils
            .GetFileLines("input4.txt")
            .Map(x=>x.Split(",").SelectMany(y=>y.Split("-")))
            .Map(Utils.ToIntList)
            .Count(x => PartialOverlap(x[0],x[1],x[2],x[3]))
            .Print();
    }

    public static void Main(string[] args)
    {
        day4.pt1();
    }
}

