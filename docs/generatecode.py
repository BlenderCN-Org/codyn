#!/usr/bin/python

import sys, re

def embed_code(filename):
    c = open(filename).read()
    return '''```cdn
{0}
```'''.format(c)

excode = re.compile('<<<([^>]*)>>>', re.M)

contents = open(sys.argv[1]).read()
print(excode.sub(lambda x: embed_code(x.group(1)), contents))

# vi:ts=4:et
