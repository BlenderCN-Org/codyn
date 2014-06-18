#!/usr/bin/python

import sys

lines = sys.stdin.read().splitlines()
inerr = False

for line in lines:
    if inerr:
        if line.strip():
            inerr = False
    elif line and line[0] == '!':
        inerr = True

    if inerr:
        sys.stderr.write('{0}\n'.format(line))

sys.exit(1)
