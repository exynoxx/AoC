f = open('dag1.txt', 'r')
alllines = f.readlines()  # put the lines to a variable (list).

count = 0
for i in range(1, len(alllines)):
    if int(alllines[i]) > int(alllines[i - 1]):
        count += 1
print(count)

windowlist = []
for i in range(1, len(alllines) - 1):
    num = 0
    for j in range(i - 1, i + 2):
        num += int(alllines[j])
    windowlist.append(num)

count = 0
for i in range(1, len(windowlist)):
    if windowlist[i] > windowlist[i - 1]:
        count += 1

print(count)
