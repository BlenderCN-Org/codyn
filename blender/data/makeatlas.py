#!/usr/bin/env python

import subprocess, json, os, platform

if platform.system() == 'Darwin':
    inkscape = '/Applications/Inkscape.app/Contents/Resources/bin/inkscape'
else:
    inkscape = 'inkscape'

q = subprocess.check_output([inkscape, '-S', 'atlas.svg']).splitlines()
ret = {}

for area in q:
    parts = area.split(',')

    name = parts[0]

    if not name.startswith('r_'):
        continue

    name = name[2:]

    a = [int(float(x)) for x in parts[1:]]
    ret[name] = a

f = open('gui.atlas', 'w')
json.dump({'regions': ret, 'filename': 'gui.png'}, f)
f.close()

dn = open(os.devnull, 'w')
subprocess.call([inkscape, '--export-png=gui.png', '-C', 'atlas.svg'], stdout=dn)

# vi:ts=4:et
