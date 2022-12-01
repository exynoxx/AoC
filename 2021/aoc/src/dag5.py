f = open('dag5.txt', 'r')
alllines = f.readlines()

lines = [x.split(" -> ") for x in alllines]
lines = [x[0].split(",") + x[1].split(",") for x in lines]
lines = [(int(a), int(b), int(c), int(d)) for a, b, c, d in lines]

maxx = max(max(x) for x in lines)

grid = [[0 for _ in range(maxx + 1)] for _ in range(maxx + 1)]

for x1, y1, x2, y2 in lines:
    if x1 == x2:
        for y in range(min(y1, y2), max(y1, y2) + 1):
            grid[y][x1] += 1
    elif y1 == y2:
        for x in range(min(x1, x2), max(x1, x2) + 1):
            grid[y1][x] += 1
    else:
        x = list(range(x1, x2 + 1)) if x1 < x2 else list(reversed(range(x2, x1 + 1)))
        y = list(range(y1, y2 + 1)) if y1 < y2 else list(reversed(range(y2, y1 + 1)))
        for i in range(len(x)):
            grid[y[i]][x[i]] += 1

# for row in grid:
#    print(*row)

count = 0
for x in range(maxx + 1):
    for y in range(maxx + 1):
        if grid[x][y] > 1:
            count += 1

print(count)
