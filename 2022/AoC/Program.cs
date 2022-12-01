/*string? line = Console.ReadLine();


var max = 0;
var current = 0;
while (line != null)
{
    
}*/

using AoC;

//pt1
/*Utils.GetFileLines("input1.txt")
    .ToList()
    .Partition(x => x == "")
    .Select(x => x.Select(int.Parse).Sum())
    .Max()
    .Print();*/
    
//pt2
Utils.GetFileLines("input1.txt")
    .ToList()
    .Partition(x => x == "")
    .Select(x => x.Select(int.Parse).Sum())
    .OrderByDescending(x=>x)
    .Take(3)
    .Sum()
    .Print();