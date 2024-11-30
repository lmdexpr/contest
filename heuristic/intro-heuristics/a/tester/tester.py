#!/usr/bin/env python3

import sys
import math

def read_input(file):
	with open(file) as f:
		D = int(f.readline())
		c = list(map(int, f.readline().split()))
		s = [list(map(int, f.readline().split())) for _ in range(D)]
		return D, c, s

def read_output(file):
	with open(file) as f:
		out = [int(f.readline()) for _ in range(D)]
		return out

if len(sys.argv) != 3:
	print("Usage: {} <input_file> <output_file>".format(sys.argv[0]))
	exit(1)

D, c, s = read_input(sys.argv[1])
out = read_output(sys.argv[2])

sat = 0
last = [0] * 26
for d in range(D):
	assert(1 <= out[d] and out[d] <= 26)
	j = out[d] - 1
	last[j] = d + 1
	for i in range(26):
		sat -= (d + 1 - last[i]) * c[i]
	sat += s[d][j]
	
print("satisfaction: {}".format(sat))
print("score: {}".format(max(0, 1000000 + sat)))



def color(val):
	tmp = int((-math.cos(2 * math.pi * val) / 2 + 0.5) * 255)
	if val >= 0.5:
		return 255, tmp, 0
	else:
		return tmp, 255, 0

with open("vis.svg", mode = 'w') as f:
	f.write('<?xml version="1.0"?>')
	f.write('<svg xmlns="http://www.w3.org/2000/svg">')
	for i in range(26):
		r, g, b = color(c[i] / 100)
		f.write('<rect x="{}" y="{}" width="40" height="10" fill="#{:02x}{:02x}{:02x}" stroke="grey" stroke-width="1"/>'.format(i * 40 + 1, 1, r, g, b))
		f.write('<text x="{}" y="{}" font-size="7">{}</text>'.format(i * 40 + 10, 9, c[i]))
	for d in range(D):
		for i in range(26):
			r, g, b = color(s[d][i] / 20000)
			f.write('<rect x="{}" y="{}" width="40" height="10" fill="#{:02x}{:02x}{:02x}" stroke="grey" stroke-width="1"/>'.format(i * 40 + 1, 20 + 10 * d + 1, r, g, b))
			f.write('<text x="{}" y="{}" font-size="7">{}</text>'.format(i * 40 + 10, 29 + 10 * d, s[d][i]))
	for d in range(D):
		f.write('<rect x="{}" y="{}" width="40" height="10" fill="none" stroke="black" stroke-width="3"/>'.format((out[d] - 1) * 40 + 1, 20 + 10 * d + 1))
	f.write('</svg>')
