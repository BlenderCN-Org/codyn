#!/usr/bin/env python

import sys

keywords = [x.strip() for x in open('keywords.list').readlines()]
selectors = [x.strip() for x in open('selectors.list').readlines()]

c = sys.stdin.read()

indent = '  ' * 2

c = c.replace('@KEYWORDS@', ',%\n'.join([indent + x for x in keywords]).strip())
c = c.replace('@SELECTORS@', ',%\n'.join([indent + x for x in selectors]).strip())

sys.stdout.write(c)
