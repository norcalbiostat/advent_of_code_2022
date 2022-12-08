s = 0
ID = 1
max_calories = {}
with open("input.txt") as f:
    for line in f:
        if len(line) > 1:
            s += int(line)
        else:
            max_calories[ID] = s
            s = 0
            ID += 1

print(sorted(max_calories.items(), key = lambda x: x[1])[-3:])
