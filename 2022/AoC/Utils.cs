using System.Text.RegularExpressions;

namespace AoC;

public static class Utils
{
    public static IEnumerable<string> GetConsoleLines()
    {
        string line = Console.ReadLine();
        while (( line = Console.ReadLine()) != "x") 
            yield return line;
    }
    
    public static IEnumerable<string> GetFileLines(string file)
    {
        return File.ReadLines(Path.Combine("../../../",file));
    }

    public static void ForEach<T>(this IEnumerable<T> list, Action<T> f)
    {
        foreach (var x in list)
        {
            f(x);
        }
    }

    public static IEnumerable<G> Map<T,G>(this IEnumerable<T> list, Func<T,G> pred)
    {
        return list.Select(pred);
    }

    public static List<int> ToIntList(this IEnumerable<string> list)
    {
        return list.Select(int.Parse).ToList();
    }

    public static IEnumerable<IEnumerable<T>> Partition<T>(this IList<T> source, Func<T,bool> pred)
    {
        var i = 0;
        var j = 0;
        foreach (var x in source)
        {
            if (pred(x))
            {
                yield return source.Take(new Range(i, j));
                i = j+1;
            }

            j++;
        }
    }
    

    /*public static IEnumerable<IEnumerable<string>> RegexMatch(this string input,string pattern)
    {
        var regex = new Regex(pattern);
        var matches = regex.Matches(input);
        return matches.Select(x => x.Groups.Values.Select(y => y.Value));
    }

    private static string PatternMatch(string raw)
    {
        return raw.Replace(@"", "");

    }
    
    

    public static IEnumerable<IEnumerable<string>> GetRegexConsoleLines(string pattern)
    {
        return string.Join("", GetConsoleLines()).RegexMatch(pattern);
    }
    */

    public static string ToStringg(this IEnumerable<string> list, string sep = ",")
    {
        return string.Join(sep, list);
    }
    
    public static void Print(this IEnumerable<string> list)
    {
        Console.WriteLine(list.ToStringg());
    }
    public static void Print(this object o)
    {
        Console.WriteLine(o);
    }
}