from collections import defaultdict

lines = open('day14.txt', 'r').readlines()
query = lines[0]
rules = {}
for i in range(2, len(lines)):
    a, b = lines[i].strip().split(" -> ")
    rules[a] = b

num_instanses_dict_a = defaultdict(int)

for i in range(len(query) - 1):
    pair = query[i] + query[i + 1]
    if pair in rules:
        num_instanses_dict_a[pair] += 1
num_instanses_dict_b = num_instanses_dict_a.copy()

for iter in range(10):
    for rule, replacement in rules.items():
        num_instanses = num_instanses_dict_a[rule]
        if num_instanses == 0:
            continue

        a1 = rule[0] + replacement
        a2 = replacement + rule[1]

        # before: nn nc cb
        # nn -> c
        # after nc .cn. nc cb

        num_instanses_dict_b[rule] = 0
        if a1 in rules:
            num_instanses_dict_b[a1] += num_instanses
        if a2 in rules:
            num_instanses_dict_b[a2] += num_instanses

    num_instanses_dict_a = num_instanses_dict_b
    num_instanses_dict_b = num_instanses_dict_b.copy()

    print(*[str(k) + ":" + str(v) for k, v in num_instanses_dict_a.items() if v > 0])

count = defaultdict(int)
for key in num_instanses_dict_a.keys():
    count[key[0]]+=1

maxx = max(count.values())
minn = min(count.values())
print(maxx - minn)
