using AoC;

static class day2
{
    private static readonly int[,] outcome = new int[3,3]
    {
        {3, 6, 0},
        {0, 3, 6},
        {6, 0, 3}
    };

    public static void pt1()
    {
        var hs = new Dictionary<string, int>
        {
            {"X", 1}, {"Y", 2}, {"Z", 3},
            {"A", 1}, {"B", 2}, {"C", 3}
        };

        Utils
            .GetFileLines("input2.txt")
            .Select(line =>
            {
                var split = line.Split(" ");
                var (opponent,me) = (hs[split[0]],hs[split[1]]);
                return outcome[opponent-1,me-1] + me;
            })
            .Sum()
            .Print();
    }

    public static void pt2()
    {
        var hs = new Dictionary<string, int>
        {
            {"X", 0}, {"Y", 3}, {"Z", 6},
            {"A", 1}, {"B", 2}, {"C", 3}
        };
        
        Utils
            .GetFileLines("input2.txt")
            .Select(line =>
            {
                var split = line.Split(" ");
                var (opponent,choice) = (hs[split[0]],hs[split[1]]);

                var me = Enumerable.Range(1, 3).Single(i => outcome[opponent-1, i-1] == choice);
                return outcome[opponent-1,me-1] + me;
            })
            .Sum()
            .Print();
        
    }

}