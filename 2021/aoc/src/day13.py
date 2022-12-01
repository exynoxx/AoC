coords = []
commands = []
for line in open('day13.txt', 'r').readlines():
    if "," in line:
        x, y = line.split(",")
        coords.append((int(x), int(y)))
    else:
        splitt = line.split(" ")
        if len(splitt) < 3:
            continue
        _, _, ins = line.split(" ")
        axis, val = ins.split("=")
        commands.append((axis, int(val)))
maxy = max(y[1] for y in coords) + 1
maxx = max(x[0] for x in coords) + 1

grid = [[0 for _ in range(maxy)] for _ in range(maxx)]
for x, y in coords:
    grid[x][y] = 1

endx = len(grid)
endy = len(grid[0])


# xline
def foldl(grid, line):
    global endx
    for x in range(line):
        for y in range(len(grid[0])):
            grid[x][y] = max(grid[x][y], grid[endx - x - 1][y])
    for x in range(line,len(grid)):
        for y in range(len(grid[0])):
            grid[x][y] = 0
    endx = line
    return grid


# yline
def foldu(grid, line):
    global endy
    for x in range(len(grid)):
        for y in range(line):
            grid[x][y] = max(grid[x][y], grid[x][endy - y - 1])
    for x in range(len(grid)):
        for y in range(line,len(grid[0])):
            grid[x][y] = 0
    endy = line
    return grid


def count(grid):
    return sum(row.count(1) for row in grid)


for axis, line in commands:
    if axis == 'x':
        foldl(grid, line)
    else:
        foldu(grid, line)

for i in range(len(grid)):
    print(*["#" if e == 1 else " " for e in [r[i] for r in grid]])
