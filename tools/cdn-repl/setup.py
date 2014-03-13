#!/usr/bin/env python

from setuptools import setup

import sys

sys.executable = '/usr/bin/cdn-python'

setup(name='cdn-repl',
      version='3.0',
      description='codyn repl',
      author='Jesse van den Kieboom',
      author_email='jesse@codyn.net',
      url='http://www.codyn.net',
      license='LGPLv2',
      keywords=['codyn', 'repl'],
      packages=['cdnrepl'],
      entry_points = {
          'console_scripts': [
              'cdn-repl = cdnrepl:run'
          ]
      }
)

# vi:ts=4:et
