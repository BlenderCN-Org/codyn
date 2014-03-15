#!/usr/bin/env/python

c = open('cdnrepl/plot.html.in').read()
c = c.replace('@JQUERY_JAVASCRIPT@', open('cdnrepl/jquery.js').read())
c = c.replace('@REPL_CSS@', open('cdnrepl/plot.css').read())
c = c.replace('@REPL_JAVASCRIPT@', open('cdnrepl/plot.js').read())

with open('cdnrepl/plot.html', 'w') as f:
    f.write(c)
