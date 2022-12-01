a = list(map(int, open('day7.txt', 'r').readlines()[0].split(",")))
a.sort()
center = a[len(a)//2]
print(sum((abs(center-e)*(abs(center-e)+1))//2 for e in a))

print(min(sum((abs(center-e)*(abs(center-e)+1))//2 for e in a) for center in range(1000)))