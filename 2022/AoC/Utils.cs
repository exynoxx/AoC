using System.Collections;
using System.Runtime.CompilerServices;
using System.Text;
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
    
    public static void WriteLines(string file, IEnumerable<string> lines)
    {
        File.WriteAllLines(Path.Combine("../../../",file), lines, Encoding.UTF8);
    }
    
    public static TValue GetOrCreate<TKey, TValue>(this IDictionary<TKey, TValue> dict, TKey key) where TValue : new()
    {
        if (!dict.TryGetValue(key, out TValue val))
        {
            val = new TValue();
            dict.Add(key, val);
        }

        return val;
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
    
    public static int ToInt(this string s)
    {
        return int.Parse(s);
    }
    
    public static LinkedList<T> ToLinkedList<T>(this IEnumerable<T> list)
    {
        return new LinkedList<T>(list);
    }

    public static Stack<T> ToStack<T>(this IEnumerable<T> list)
    {
        return new Stack<T>(list);
    }
    
    public static PriorityQueue<T, int> ToPriorityQueue<T>(this IEnumerable<(T,int)> list)
    {
        return new PriorityQueue<T, int>(list);
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

    public class LambdaCmp<T> : IComparer<T>
    {
        private Func<T, T, int> l;

        public LambdaCmp(Func<T, T, int> l)
        {
            this.l = l;
        }

        public int Compare(T? x, T? y)
        {
            return l(x, y);
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
    
    public static T[,] Make2DArray<T>(this T[][] input)
    {
        var w = input.Length;
        var h = input[0].Length;
        T[,] output = new T[w, h];
        for (int i = 0; i < w; i++)
        {
            for (int j = 0; j < h; j++)
            {
                output[i, j] = input[i][j];
            }
        }
        return output;
    }

    public static string ToStringg(this IEnumerable<string> list, string sep = ",")
    {
        return string.Join(sep, list);
    }
    
    public static string Join(this IEnumerable<string> list, string sep = ",")
    {
        return string.Join(sep, list);
    }
    
    public static string Join(this IEnumerable<char> list, string sep = ",")
    {
        return string.Join(sep, list);
    }
    
    public static void Printl<T>(this IEnumerable<T> list)
    {
        Console.WriteLine(list.Select(x=>x.ToString()).ToStringg());
    }
    public static void Print(this object o)
    {
        Console.WriteLine(o);
    }
}