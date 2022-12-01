f = open('day10.txt', 'r')
q = []
s = 0
table = {')': 3, ']': 57, '}': 1197, '>': 25137}
invert = {'(': ')', '[': ']', '{': '}', '<': '>'}

incomplete_lines = []

for line in f.readlines():
    try:
        for c in line.strip():
            if c == "(" or c == "[" or c == "{" or c == "<":
                q.append(invert[c])
            else:
                expected = q[-1]
                if c != expected:
                    print("expected", expected, "found", c)
                    s += table[c]
                    raise Exception
                else:
                    q = q[:-1]
        incomplete_lines.append((line, q[::-1]))
        q = []
    except:
        q = []
        continue
print(s)

newcost = {')': 1, ']': 2, '}': 3, '>': 4}
scores = []
for line, q in incomplete_lines:
    s = 0
    for e in q:
        s *= 5
        s += newcost[e]
    scores.append(s)
scores.sort()
print(scores[len(scores)//2])