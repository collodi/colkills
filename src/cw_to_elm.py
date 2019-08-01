import sys

codewords = None
with open(sys.argv[1], 'r') as f:
	codewords = [list(ln.strip()) for ln in f]

print('module Codewords exposing (codewords32)')
print('codewords32 : List (List Bool)')
print('codewords32 =')

print('    [')

for i, cw in enumerate(codewords):
	cw = ['True' if x == '1' else 'False' for x in cw]
	print('        [' + ','.join(cw) + ']', end='')

	if i < len(codewords) - 1:
		print(',')
	else:
		print()

print('    ]')
