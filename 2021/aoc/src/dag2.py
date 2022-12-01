horizontal = 0
depth = 0
aim = 0

with open('dag2.txt', 'r') as f: #open the file
    contents = f.readlines() #put the lines to a variable (list).

    for l in contents:
        name, val = l.split(" ")
        if name == "forward":
            horizontal += int(val)
            depth += aim*int(val)
        elif name == "down":
            aim += int(val)
        elif name == "up":
            aim -= int(val)

print(horizontal, depth, horizontal*depth)