from math import prod

steps = []
for line in open('day22.txt', 'r').readlines():
    mode, rest = line.strip().split(" ")
    xyz = rest.replace("x=","").replace("y=","").replace("z=","").replace("..",",").split(",")
    mode = 1 if mode == "on" else -1
    intlist = list(map(int,xyz))
    steps.append(tuple(intlist+[mode]))

def size(cub):
    if not cub: return 0
    return cub[6] * prod(cub[i + 1] - cub[i] + 1 for i in range(0, 6, 2))

def not_overlapping(cub1, cub2):
    return any(cub1[i + 1] < cub2[i] or cub1[i] > cub2[i + 1] for i in range(0, 6, 2))

def flatten_listOfTuples(l):
    return list(sum(l, ()))

def intersection(cub1, cub2, flip=True):
    if not_overlapping(cub1, cub2):
        return None

    ret = flatten_listOfTuples([(max(cub1[i], cub2[i]), min(cub1[i + 1], cub2[i + 1])) for i in range(0, 6, 2)])
    ret.append(-cub2[6] if flip else cub2[6])
    return tuple(ret)


window = (-50, 50, -50, 50, -50, 50, 0)
inside = []

for A in steps:
    if not (A:=intersection(window, A, flip=False)): continue
    inside.extend([x for B in inside if (x := intersection(A, B))])
    inside.extend([A] if A[6] == 1 else [])
print(sum(size(e) for e in inside))
