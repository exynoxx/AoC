from collections import defaultdict

s1 = "(B,C) (C,D) (B,A)"
s2 = "(D,E) (F,G) (B,H) (A,B) (B,C) (C,D) (E,F)"

def do(s):
    input_list = [e.split(",") for e in s.replace("(", "").replace(")", "").split(" ")]

    children = defaultdict(list)
    parent = {}

    for tup in reversed(input_list):
        parent[tup[1]] = tup[0]
        children[tup[0]].append(tup[1])

    def recurse(x):
        match children[x]:
            case []:
                return "(" + x + ")"
            case [a]:
                return "(" + x + recurse(a) + ")"
            case [a, b]:
                return "(" + x + recurse(a) + recurse(b) + ")"

    root = input_list[0][0]
    while root in parent:
        root = parent[root]

    return recurse(root)
print("input", s1)
print("output", do(s1))
print("input", s2)
print("output", do(s2))