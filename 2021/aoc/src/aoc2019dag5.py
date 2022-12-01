initram = list(map(int, open('aoc2019dag5.txt', 'r').readlines()[0].split(",")))


def compute(ram):
    ip = 0
    while ip < len(ram):
        op = int(str(ram[ip])[-2:])
        args = [int(c) for c in str(ram[ip])[:-2][::-1]]
        if op == 99:
            break
        elif op == 1:
            op, src1, src2, dest = ram[ip:ip + 4]
            arg1 = src1
            arg2 = src2
            if len(args) == 0 or args[0] == 0:
                arg1 = ram[src1]
            if len(args) < 2 or args[1] == 0:
                arg2 = ram[src2]

            tmp = arg1 + arg2
            ram[dest] = tmp
            ip += 4
        elif op == 2:
            op, src1, src2, dest = ram[ip:ip + 4]
            arg1 = src1
            arg2 = src2
            if len(args) == 0 or args[0] == 0:
                arg1 = ram[src1]
            if len(args) < 2 or args[1] == 0:
                arg2 = ram[src2]

            tmp = arg1 * arg2
            ram[dest] = tmp
            ip += 4
        elif op == 3:
            op, addr = ram[ip:ip + 2]
            val = int(input())
            ram[addr] = val
            ip += 2
        elif op == 4:
            op, addr = ram[ip:ip + 2]
            print(ram[addr])
            ip += 2
        elif op == 5:
            op, c, val = ram[ip:ip + 3]
            if c != 0:
                if len(args) < 2 or args[1] == 0:
                    ip = ram[val]
                else:
                    ip = val
        elif op == 6:
            op, c, val = ram[ip:ip + 3]
            if c == 0:
                if len(args) < 2 or args[1] == 0:
                    ip = ram[val]
                else:
                    ip = val
        elif op == 7:
            op, arg1, arg2, arg3 = ram[ip:ip + 4]
            if len(args) == 0 or args[0] == 0:
                arg1 = ram[arg1]
            if len(args) < 2 or args[1] == 0:
                arg2 = ram[arg2]

            if arg1 < arg2:
                ram[arg3] = 1
            else:
                ram[arg3] = 0

            ip += 4
        elif op == 8:
            op, arg1, arg2, arg3 = ram[ip:ip + 4]
            if len(args) == 0 or args[0] == 0:
                arg1 = ram[arg1]
            if len(args) < 2 or args[1] == 0:
                arg2 = ram[arg2]

            if arg1 == arg2:
                ram[arg3] = 1
            else:
                ram[arg3] = 0

            ip += 4
        else:
            print("op=", op, "ERRROR!!!!")
            break

    return ram


compute(initram.copy())
