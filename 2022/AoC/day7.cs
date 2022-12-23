using AoC;

static class day7
{
    private interface Node
    {
        public Node Parent { get; }
        public string Name { get; }
    }

    private record File(Node Parent, string Name, int size) : Node;

    private record Dir(Node Parent, string Name, Dictionary<string, Node> children) : Node;

    private static (int size, List<int> folderSizes) DirSize(Node node)
    {
        if (node is Dir dir)
        {
            var children = dir.children
                .Select(x => x.Value)
                .Map(DirSize);

            var sum = children.Sum(x => x.size);

            var folders = children
                .SelectMany(x => x.folderSizes)
                .ToList();
            folders.Add(sum);
            return (sum, folders);
        }

        var file = node as File;
        return (file.size, new List<int>());
    }

    static Node BuildTree(IEnumerable<string> lines)
    {
        Node current = null;
        foreach (var line in lines)
        {
            var lsplit = line.Split(" ");
            if (lsplit[0] == "$")
            {
                if (lsplit[1] == "cd")
                {
                    var folder = lsplit[2];
                    if (folder == "/")
                    {
                        if (current == null)
                        {
                            current = new Dir(null, "/", new());
                        }
                        else
                        {
                            while (current.Parent != null) current = current.Parent;
                        }
                    }
                    else if (folder == "..")
                    {
                        current = current.Parent;
                    }
                    else
                    {
                        var currentdir = current as Dir;
                        if (currentdir == null) throw new Exception("a");
                        current = currentdir.children[folder];
                    }
                }
                //if (lsplit[1] == "ls")
            }
            else
            {
                var currentdir = current as Dir;
                if (currentdir == null) throw new Exception("b");
                if (lsplit[0] == "dir")
                {
                    currentdir.children[lsplit[1]] = new Dir(current, lsplit[1], new Dictionary<string, Node>());
                }
                else
                {
                    var filename = lsplit[1];
                    currentdir.children[filename] = new File(current, filename, lsplit[0].ToInt());
                }
            }
        }

        while (current.Parent != null)
            current = current.Parent;

        return current;
    }

    public static void pt1()
    {
        var root = BuildTree(Utils.GetFileLines("input7.txt"));
        
        DirSize(root)
            .folderSizes
            .Where(x => x < 100000)
            .Sum()
            .Print();
    }
    
    public static void pt2()
    {
        var root = BuildTree(Utils.GetFileLines("input7.txt"));
        var limit = 30000000;
        var total = 70000000;
        
        var (max,sizes) = DirSize(root);

        sizes
            .Where(x=>total - max + x >limit)
            .Min()
            .Print();
    }
}