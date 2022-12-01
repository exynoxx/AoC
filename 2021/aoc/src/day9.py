from math import prod

grid = [list(map(int, [c for c in line.strip()])) for line in open('day9.txt', 'r').readlines()]

def p(x, y):
    if x < 0 or y < 0:
        return 10
    try:
        return grid[x][y]
    except:
        return 10


lowpoints = []
for i in range(len(grid)):
    for j in range(len(grid[i])):
        if p(i, j) < p(i - 1, j) and p(i, j) < p(i + 1, j) and p(i, j) < p(i, j - 1) and p(i, j) < p(i, j + 1):
            lowpoints.append((i, j))

visited = set()
def dfs(x, y):
    if p(x, y) >= 9 or (x,y) in visited:return 0
    visited.add((x,y))
    return 1 + dfs(x - 1, y) + dfs(x + 1, y) + dfs(x, y - 1) + dfs(x, y + 1)


basins = sorted([dfs(i, j) for i, j in lowpoints], reverse=True)
print(prod(basins[i] for i in range(3)))
