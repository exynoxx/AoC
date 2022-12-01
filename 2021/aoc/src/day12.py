from collections import defaultdict

adj = defaultdict(list)
smallcaves = set()
for line in open('day12.txt', 'r').readlines():
    x, y = line.strip().split("-")
    adj[x].append(y)
    adj[y].append(x)
    if x.islower():
        smallcaves.add(x)
    if y.islower():
        smallcaves.add(y)
smallcaves.discard("start")
smallcaves.discard("end")

paths = set()
def dfs(x, current_path, visited, target):
    if x == target and visited[x] == 2 or x != target and visited[x] == 1:
        return

    if x == 'end':
        paths.add(str(current_path + ['end']))
        return

    newvisited = visited.copy()

    if x.islower():
        newvisited[x] += 1

    for v in adj[x]:
        dfs(v, current_path + [x], newvisited,target)

def pt1():
    dfs('start', [], defaultdict(int), '-1')
    print(len(paths))

def pt2():
    for target in smallcaves:
        dfs('start', [], defaultdict(int), target)
        print(len(paths))

pt1()
pt2()