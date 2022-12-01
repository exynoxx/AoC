a = list(map(int, open('day6.txt', 'r').readlines()[0].split(","))); b = [a.count(i) for i in range(9)]
for _ in range(80): b = [sum(x) for x in zip(b[1:] + [b[0]], [0, 0, 0, 0, 0, 0, b[0], 0, 0])]
print(sum(b))

"""
f = open('day6.txt', 'r')
init = list(map(int, f.readlines()[0].split(",")))

# 0 1 2 3 4 5 6 7 8
watch = [0] * 9

for e in init:
    watch[e] += 1

for i in range(80):
    zero = watch[0]
    for j in range(8):
        watch[j] = watch[j + 1]

    watch[6] += zero  # contain #0 value
    watch[8] = zero  # spawn #0 many new fish

print(sum(watch))
"""
