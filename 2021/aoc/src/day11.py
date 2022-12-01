grid = [list(map(int, [c for c in line.strip()])) for line in open('day11.txt', 'r').readlines()]


def p(x, y):
    if x < 0 or y < 0:
        return 10
    try:
        return grid[x][y]
    except:
        return 10


def increase(i, j):
    if i < 0 or j < 0 or i >= 10 or j >= 10:
        return
    grid[i][j] += 1
    if grid[i][j] == 10:
        for x in [-1, 0, 1]:
            for y in [-1, 0, 1]:
                increase(i + x, j + y)
        grid[i][j] = -100

def do():
    for it in range(1000):
        for i in range(10):
            for j in range(10):
                increase(i, j)

        success = True
        for i in range(10):
            for j in range(10):
                if grid[i][j] < 0:
                    grid[i][j] = 0
                else:
                    success = False
        if success:
            print(it+1)
            return

do()