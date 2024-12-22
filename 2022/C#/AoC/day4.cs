using AoC;

static class day4
{
    public static void pt1()
    {
        var FullyContained = (int a, int b, int x, int y) => a <= x && b >= y;
        Utils
            .GetFileLines("input4.txt")
            .Map(x=>x.Split(",").SelectMany(y=>y.Split("-")))
            .Map(Utils.ToIntList)
            .Count(x => FullyContained(x[0],x[1],x[2],x[3])||FullyContained(x[2],x[3],x[0],x[1]))
            .Print();
    }
    
    public static void pt2()
    {
        var PartialOverlap = (int a, int b, int x, int y) => !(b<x || a>y);
        Utils
            .GetFileLines("input4.txt")
            .Map(x=>x.Split(",").SelectMany(y=>y.Split("-")))
            .Map(Utils.ToIntList)
            .Count(x => PartialOverlap(x[0],x[1],x[2],x[3]))
            .Print();
    }
}