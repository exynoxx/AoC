using AoC;

static class day6
{
    public static void pt1And2()
    {
        var line = Utils.GetFileLines("input6.txt").First().ToCharArray();
        
        //pt2
        var k = 14; //pt1 = 4
        var count = new Dictionary<char, int>();
        var duplicates = 0;

        foreach (var e in line.Take(k))
        {
            var c = count.GetValueOrDefault(e,0);
            if (c > 0) duplicates++;
            count[e] = c + 1;
        }

        
        for (int i = 0, j = k; j < line.Length; i++, j++ )
        {
            if (duplicates == 0)
            {
                j.Print();
                return;
            }

            var tail = line[i];
            var head = line[j];
            
            var h = count[tail];
            if (h > 1) duplicates--;
            count[tail] = h - 1;

            var t = count.GetValueOrDefault(head, 0);
            if (t > 0) duplicates++;
            count[head] = t + 1;
        }
        
    }
}