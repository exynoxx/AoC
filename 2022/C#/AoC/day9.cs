using AoC;

static class day9
{

    private record Pos(int x, int y)
    {
        public override int GetHashCode()
        {
            return x.GetHashCode()+y.GetHashCode();
        }

        public static Pos operator +(Pos me,Pos other) => new Pos(me.x+other.x,me.y+other.y);
        public static Pos operator -(Pos me,Pos other) => new Pos(me.x-other.x,me.y-other.y);
        public int x { get; set; } = x;
        public int y { get; set; } = y;

        
        public void Follow(Pos head)
        {
            var delta = head - this;
            if (delta.x > 1 && delta.y == 0)
            {
                x+=delta.x-1;
            }
            else if (delta.x == 0 && delta.y > 1)
            {
                
            }
        }
    }
    
    public static void pt1()
    {
        var current = new Pos(0,0);
        var prev = new Pos(0, 0);
        var tail = new Pos(0, 0);
        
        var instructions = Utils
            .GetFileLines("input9.txt")
            .Map(x => x.Split(" "));
        
        var toDelta = new Dictionary<string, Pos>
        {
            {"U", new Pos(0, 1)},
            {"D", new Pos(0, -1)},
            {"L", new Pos(-1, 0)},
            {"R", new Pos(1, 0)}
        };

        var tailPositions = new HashSet<Pos>();
        
        foreach (var ins in instructions)
        {
            var d = ins.First();
            var l = ins.Last().ToInt();
            
            for (int i = 0; i < l; i++)
            {
                current += toDelta[d];
                var delta = current - tail;

                if (delta.x == 0 && delta.y>1)
                {
                    tail.y += delta.y-1;
                }else if (delta.y == 0 && delta.x>1)
                {
                    tail.x += delta.x-1;
                }
                else if (delta.x >= 1 && delta.y >= 1)
                {
                    if (delta.x > delta.y)
                    {
                        delta -= new Pos(1, 0);
                    }
                    else
                    {
                        delta -= new Pos(0, 1);

                    }
                    tail += delta;
                    tail += delta;
                }
                
                tailPositions.Add(tail);
            }
        }
        
        tailPositions.Printl();
        tailPositions.Count.Print();
        
        
    }
}