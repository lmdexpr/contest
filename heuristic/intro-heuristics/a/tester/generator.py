#!/usr/bin/env python3

import sys
import random

if len(sys.argv) != 2:
	print("Usage: {} <seed>".format(sys.argv[0]))
	exit(1)

random.seed(int(sys.argv[1]))

D = 365
c = [random.randrange(0, 101) for _ in range(26)]
s = [[random.randrange(0, 20001) for _ in range(26)] for _ in range(D)]

print(D)
print(" ".join(map(str, c)))
for sd in s:
	print(" ".join(map(str, sd)))
