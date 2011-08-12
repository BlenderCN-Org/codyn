# -*- coding: utf-8 -*-
#
#  parser.py - cpgcontext gedit plugin
#
#  Copyright (C) 2011 - Jesse van den Kieboom
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330,
#  Boston, MA 02111-1307, USA.

import threading, os, json, subprocess, tempfile, signal, fcntl, sys
import utils

class Expansion:
    class Item:
        def __init__(self, item):
            self.value = item['value']
            self.index = item['index']

    def __init__(self, expansion):
        self.items = [Expansion.Item(x) for x in expansion]

class Define:
    def __init__(self, define):
        self.key = define['key']
        self.value = define['value']

class SelectionItem:
    def __init__(self, selection):
        self.name = selection['name']
        self.typename = selection['typename']

        self.expansions = [Expansion(x) for x in selection['expansions']]
        self.defines = [Define(x) for x in selection['defines']]

class Selection:
    def __init__(self, selection):
        self.ins = [SelectionItem(x) for x in selection['in']]
        self.outs = [SelectionItem(x) for x in selection['out']]

class Context:
    def __init__(self, context):
        self.line_start = context['line_start']
        self.line_end = context['line_end']
        self.column_start = context['column_start']
        self.column_end = context['column_end']

        self.selections = [Selection(x) for x in context['selections']]

class Error:
    def __init__(self, error):
        self.message = error['message']
        self.line = error['line']
        self.line_start = error['line_start']
        self.line_end = error['line_end']
        self.column_start = error['column_start']
        self.column_end = error['column_end']

class Parser:
    def __init__(self, doc, finishedcb):
        self.doc = doc
        self.contexts = []

        self.finishedcb = finishedcb

        self.filename = None
        self.istmp = False
        self.pipe = None

        if doc.get_modified() or doc.is_untitled():
            loc = doc.get_location()

            if loc:
                dirname = loc.get_parent()
                basename = loc.get_basename()

                f = dirname.get_child("." + basename + ".parse-tmp").get_path()
                fp = file(f, 'w')
            else:
                fp = tempfile.NamedTemporaryFile(mode='w', delete=False)
                f = fp.name

            bounds = doc.get_bounds()
            fp.write(doc.get_text(bounds[0], bounds[1], True))
            fp.close()

            self.filename = os.path.realpath(f)
            self.istmp = True
        else:
            self.filename = doc.get_location().get_path()

    def cancel(self):
        self.finishedcb = None

        if self.pipe:
            os.kill(self.pipe.pid, signal.SIGTERM)
            self.pipe = None

    def run(self):
        dirname = os.path.dirname(self.filename)
        filename = os.path.basename(self.filename)

        self.ret = None

        try:
            self.pipe = subprocess.Popen(['cpg-context', filename],
                                         cwd=dirname,
                                         stdout=subprocess.PIPE)
        except Exception as (e):
            sys.stderr.write('Failed to execute cpg-context: %s\n' % (e,))
            self.pipe = None
            return False

        self.read_buffer = ''

        flags = fcntl.fcntl(self.pipe.stdout.fileno(), fcntl.F_GETFL) | os.O_NONBLOCK
        fcntl.fcntl(self.pipe.stdout.fileno(), fcntl.F_SETFL, flags)

        utils.io_add_watch(self.pipe.stdout, self.on_output)
        utils.child_watch_add(self.pipe.pid, self.on_parser_end)

    def on_parser_end(self, pid, error_code):
        try:
            self.ret = json.loads(self.read_buffer)
        except Exception as (e):
            sys.stderr.write('Failed to load json: %s\n' % (e,))
            self.ret = None

        self.pipe = None
        self.read_buffer = ''

        if self.ret and self.ret['status'] == 'ok':
            # Filter data so we just keep our own
            self.ret['data'] = map(lambda x: Context(x), filter(lambda x: x['filename'] == self.filename, self.ret['data']))
        elif self.ret and self.ret['status'] == 'error':
            self.ret['data'] = Error(self.ret['data'])

        if self.istmp:
            try:
                os.unlink(self.filename)
            except:
                pass

        if self.finishedcb:
            utils.idle_add(self.idle_finished)

    def idle_finished(self):
        if self.finishedcb:
            self.finishedcb(self.ret)

        return False

    def on_output(self, source, condition):
        if utils.io_is_in(condition):
            line = source.read()

            if len(line) > 0:
                try:
                    line = unicode(line, 'utf-8')
                except:
                    line = unicode(line, locale.getdefaultlocale()[1], 'replace')

                self.read_buffer += line
        elif utils.io_is_close(condition):
            return False

        return True

# vi:ex:ts=4:et
