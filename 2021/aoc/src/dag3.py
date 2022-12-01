gamma = ""
epsilon = ""

f = open('dag3.txt', 'r')
alllines = f.readlines()  # put the lines to a variable (list).

for i in range(len(alllines[0].replace("\n", ""))):
    col = [l[i] for l in alllines]
    zero = col.count('0')
    one = col.count('1')

    if zero > one:
        gamma += '0'
        epsilon += '1'
    else:
        gamma += '1'
        epsilon += '0'

gamma = int(gamma, 2)
epsilon = int(epsilon, 2)

print(gamma * epsilon)

def reducer(candidates, cmp):
    lenn = len(candidates[0].replace("\n", ""))
    for i in range(lenn):
        col = [l[i] for l in candidates]
        zero = col.count('0')
        one = col.count('1')
        mostCommonToken = '0' if cmp(zero, one) else '1'
        candidates = list(filter(lambda line: line[i] == mostCommonToken, candidates))
        if len(candidates) == 1:
            break
    return int(candidates[0],2)


oxygen = reducer(alllines.copy(), lambda x, y: x > y)
co2 = reducer(alllines.copy(), lambda x, y: x <= y)

print("oxygen",oxygen)
print("co2", co2)
print("product", oxygen*co2)
