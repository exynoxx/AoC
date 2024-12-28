using AoC;

static class day1
{
    public static void pt1()
    {
        //pt1
        Utils.GetFileLines("input1.txt")
            .ToList()
            .Partition(x => x == "")
            .Select(x => x.Select(int.Parse).Sum())
            .Max()
            .Print();
    }

    public static void pt2()
    {
        //pt2
        Utils.GetFileLines("input1.txt")
            .ToList()
            .Partition(x => x == "")
            .Select(x => x.Select(int.Parse).Sum())
            .OrderByDescending(x=>x)
            .Take(3)
            .Sum()
            .Print();
    }
}