# -*- coding: utf-8 -*-
#
#  parser.py - cdncontext gedit plugin
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

import os, json, subprocess, tempfile, signal, fcntl, sys

from gi.repository import GObject

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
    def __init__(self, error, filename, real_filename):
        self.message = error['message']
        self.line = error['line']
        self.line_start = error['line_start']
        self.line_end = error['line_end']
        self.column_start = error['column_start']
        self.column_end = error['column_end']

        b = os.path.basename(filename)
        self.message = self.message.replace(b, real_filename)

    def __str__(self):
        return '{0}:{1}: {2}'.format(self.line, self.column_start, self.message)

class Parser:
    def __init__(self, doc, finishedcb):
        self.doc = doc
        self.contexts = []

        self.finishedcb = finishedcb

        self.filename = None
        self.istmp = False
        self.pipe = None
        self.cancelled = False
        self.idle_finished_id = None

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
            t = doc.get_text(bounds[0], bounds[1], True)
            fp.write(t)

            if len(t) == 0 or t[-1] != '\n':
                fp.write('\n')

            fp.close()

            self.filename = os.path.realpath(f)
            self.istmp = True
        else:
            self.filename = doc.get_location().get_path()

        if not doc.is_untitled():
            self.real_filename = doc.get_location().get_basename()
        else:
            self.real_filename = '(current document)'

    def cancel(self):
        if self.cancelled:
            return

        if not self.idle_finished_id is None:
            GObject.source_remove(self.idle_finished_id)
            self.idle_finished_id = None

        self.finishedcb = None
        self.cancelled = True

        if self.pipe:
            try:
                os.kill(self.pipe.pid, signal.SIGTERM)
            except:
                pass

            self.pipe = None

    def run(self, line = 1, column = 1):
        dirname = os.path.dirname(self.filename)
        filename = os.path.basename(self.filename)

        self.ret = None

        try:
            self.pipe = subprocess.Popen(['cdn-context', filename, '-l', str(line), '-c', str(column)],
                                         cwd=dirname,
                                         stdout=subprocess.PIPE)
        except Exception as (e):
            sys.stderr.write('Failed to execute cdn-context: %s\n' % (e,))
            self.pipe = None
            return False

        self.read_buffer = ''

        flags = fcntl.fcntl(self.pipe.stdout.fileno(), fcntl.F_GETFL) | os.O_NONBLOCK
        fcntl.fcntl(self.pipe.stdout.fileno(), fcntl.F_SETFL, flags)

        GObject.io_add_watch(self.pipe.stdout, GObject.IO_IN | GObject.IO_HUP, self.on_output)
        GObject.child_watch_add(self.pipe.pid, self.on_parser_end)

    def on_parser_end(self, pid, error_code):
        self.idle_finished_id = GObject.idle_add(self.idle_finished)

    def idle_finished(self):
        if not self.cancelled:
            try:
                self.ret = json.loads(self.read_buffer)
            except Exception as (e):
                sys.stderr.write('Failed to load json: %s\n' % (e,))
                self.ret = None
        else:
            self.ret = None

        self.pipe = None
        self.read_buffer = ''

        if self.ret and self.ret['status'] == 'ok':
            # Filter data so we just keep our own
            self.ret['data'] = map(lambda x: Context(x), filter(lambda x: x['filename'] == os.path.realpath(self.filename), self.ret['data']))
        elif self.ret and self.ret['status'] == 'error':
            self.ret['data'] = Error(self.ret['data'], self.filename, self.real_filename)

        if self.istmp:
            try:
                os.unlink(self.filename)
            except:
                pass

        if self.finishedcb:
            self.finishedcb(self.ret)

        return False

    def on_output(self, source, condition):
        if self.cancelled:
            return False

        if condition & (GObject.IO_IN | GObject.IO_PRI):
            line = source.read()

            if len(line) > 0:
                try:
                    line = unicode(line, 'utf-8')
                except:
                    line = unicode(line, locale.getdefaultlocale()[1], 'replace')

                self.read_buffer += line
        elif condition & ~(GObject.IO_IN | GObject.IO_PRI):
            return False

        return True

# vi:ex:ts=4:et
