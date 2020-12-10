with open("input.txt", "r") as r:
    input= r.read().split("\n")

inp = [int(x) for x in input if x] 
num_ways = [0 for i in range(max(inp) + 1)]

for item in sorted(inp):
    if item == 1:
        num_ways[item] = 1
        continue
    if item == 2:
        num_ways[item] = 2
        continue
    for i in range(1, 4):
        num_ways[item] += num_ways[item - i]
        
print(num_ways[-1])
