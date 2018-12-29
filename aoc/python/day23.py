# python2 with z3

from z3 import *
import re

nanobots_re = re.compile('''pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)''')

file = open('../resources/input-23.data', 'r')
lines = file.read().splitlines()

nanobots = [
    ((int(m.group(1)), int(m.group(2)), int(m.group(3))), int(m.group(4))) for m in map(lambda line: re.match(nanobotsRe, line), lines)
]

in_ranges = [
    Int('in_range_' + str(i)) for i in range(0, len(nanobots))
]

def zabs(v):
    return If(v >= 0, v, -v)

x, y, z = Ints('x y z')

range_count = Int('range_count')
dist_from_zero = Int('dist')

o = Optimize()

for i in range(0, len(nanobots)):
    (nx, ny, nz), nr = nanobots[i]
    o.add(in_ranges[i] == If(zabs(x - nx) + zabs(y - ny) + zabs(z - nz) <= nr, 1, 0))

o.add(range_count == sum(in_ranges))

o.add(dist_from_zero == zabs(x) + zabs(y) + zabs(z))

h1 = o.maximize(range_count)
h2 = o.minimize(dist_from_zero)

o.check()

print 'solution 2:', o.model()[dist_from_zero]
