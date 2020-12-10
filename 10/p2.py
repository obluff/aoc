from collections import defaultdict
with open("input.txt", "r") as r:
    asdf = r.read().split("\n")
da_input = [int(x) for x in asdf if x] 
num_ways = defaultdict(lambda: 0)
for item in sorted(da_input):
    if item == 1:
        num_ways[item] = 1
        continue
    if item == 2:
        num_ways[item] = 2
        continue
    for i in range(1, 4):
        num_ways[item] += num_ways[item - i]
        
num_ways[max(da_input)]