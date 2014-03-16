#!/usr/bin/env python

import sys

keywords = ['<keyword>{0}</keyword>'.format(x.strip()) for x in open('keywords.list').readlines()]
selectors = ['<keyword>{0}</keyword>'.format(x.strip()) for x in open('selectors.list').readlines()]

c = sys.stdin.read()

indent = '    ' * 3

c = c.replace('@KEYWORDS@', '\n'.join([indent + x for x in keywords]).strip())
c = c.replace('@SELECTORS@', '\n'.join([indent + x for x in selectors]).strip())

sys.stdout.write(c)
