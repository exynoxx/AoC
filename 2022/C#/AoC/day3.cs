using AoC;

static class day3
{
    public static void pt1()
    {
        Utils
            .GetFileLines("input3.txt")
            .Select(rucksack =>
            {
                var first = rucksack
                    .Substring(0, rucksack.Length / 2)
                    .ToHashSet();
               
                var second = rucksack.Substring(rucksack.Length / 2);

                foreach (var ch in second)
                {
                    if (first.Contains(ch))
                    {
                        if (ch >= 97) return ch-'a'+1;
                        return ch-'A'+27;
                    }
                }
                return 0;
            })
            .Sum()
            .Print();
    }

    public static void pt2()
    {
        Utils
            .GetFileLines("input3.txt")
            .Select((x, i) => new { Index = i, Value = x })
            .GroupBy(x => x.Index / 3)
            .Select(x => x.Select(v => v.Value).ToList())
            .Select(group =>
            {
                var common = group[0].Intersect(group[1].Intersect(group[2])).Single();

                if (common >= 97) return common-'a'+1;
                return common-'A'+27;

            })
            .Sum()
            .Print();
    }
}