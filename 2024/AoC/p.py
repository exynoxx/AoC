from collections import namedtuple
from math import sqrt
from pathlib import Path


Point = namedtuple('Point', ['y', 'x'])
Node = namedtuple('Node', ['p', 'd'])

# set up inputs
text = Path('data/day16.txt').read_text('utf-8')
board_lines = text.splitlines()
width = len(board_lines[0])
board = list(map(list, board_lines))
start, end = Point(*divmod(text.find('S'), width + 1)), Point(*divmod(text.find('E'), width + 1))

# simple cost heuristic based on distance to end node
def cost_heuristic(p: Point) -> float:
    return sqrt((p.x - end.x) ** 2 + (p.y - end.y) ** 2)

# helper function to print the grid with the path
def print_grid_with_path(grid, path):
    grid_copy = [row[:] for row in grid]
    for point in path:
        if grid_copy[point.y][point.x] not in "SE":
            grid_copy[point.y][point.x] = 'O'
    print("\n".join("".join(row) for row in grid_copy))
    print()


# walks path backwards, scans for nodes with incoming paths of equal cost, and traverses all recursively
def equal_cost_backtrace(parent: dict, node_cost: dict, current: Node) -> set[Point]:
    last_direction = current.d

    trace = {current.p}
    while current := parent.get(current):
        trace.add(current.p)
        for d in map(lambda o: (current.d + o) & 3, [1, -1]):
            test = Node(current.p, d)
            if last_direction != current.d and (node_cost.get(test, 0) - node_cost[current]) == 1000:
                trace.update(equal_cost_backtrace(parent, node_cost, parent.get(test)))

        last_direction = current.d


    return trace

# simple A star
def a_star() -> tuple[int, int]:
    t_start = Node(start, 0)

    pending: set[Node] = {t_start}
    parent: dict[Node, Node] = {}
    node_cost: dict[Node, float] = {t_start: 0.0}
    total_cost: dict[Node, float] = {t_start: cost_heuristic(t_start.p)}

    while len(pending):
        current = min(pending, key=lambda t: total_cost[t])

        if current.p == end:
            aa = equal_cost_backtrace(parent, node_cost, current)
            print_grid_with_path(board, aa)
            return int(node_cost[current]), len(aa)

        pending.remove(current)

		# scan in all directions
        for i, vector in enumerate([(0, 1), (1, 0), (0, -1), (-1, 0)]):
            neighbor = Node(Point(current.p.y + vector[0], current.p.x + vector[1]), i)
            if board[neighbor.p.y][neighbor.p.x] == '#':
                continue
			
			# assign new cost based on rotation and step
            new_cost = node_cost[current] + [0, 1000, 2000, 1000][(i - current.d) & 3] + 1
            if new_cost >= node_cost.get(neighbor, 1e13):
                continue

            parent[neighbor] = current
            node_cost[neighbor] = new_cost
            total_cost[neighbor] = new_cost + cost_heuristic(neighbor.p)

            if neighbor in pending:
                continue

            pending.add(neighbor)

    return 0, 0


lowest_cost, nodes = a_star()

print("Part 1:", lowest_cost)
print("Part 2:", nodes)