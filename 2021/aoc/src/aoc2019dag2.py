initram = list(map(int, open('aoc2019dag2.txt', 'r').readlines()[0].split(",")))

def compute(ram):
    for ip in range(0, len(ram), 4):
        try:
            op, src1, src2, dest = ram[ip:ip + 4]
        except:
            if ram[ip] == 99:
                break
            else:
                print("ERRROROROROR!!!!!!")

        if op == 99:
            break
        if op == 1:
            tmp = ram[src1] + ram[src2]
            ram[dest] = tmp
        if op == 2:
            tmp = ram[src1] * ram[src2]
            ram[dest] = tmp
    return ram

ram = compute(initram.copy())
print(ram[0])

def part2():
    print("part2")
    for verb in range(100):
        for noun in range(100):
            ram = initram.copy()
            ram[1] = noun
            ram[2] = verb
            if compute(ram)[0] == 19690720:
                print(100*noun+verb)
                return

    print("not found")

part2()

