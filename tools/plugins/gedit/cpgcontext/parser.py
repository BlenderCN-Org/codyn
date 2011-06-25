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

from gi.repository import GObject, Gio
import threading, os, json, subprocess, tempfile

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

class Selection:
    def __init__(self, selection):
        self.name = selection['name']
        self.typename = selection['typename']

        self.expansions = [Expansion(x) for x in selection['expansions']]
        self.defines = [Define(x) for x in selection['defines']]

class Context:
    def __init__(self, context):
        self.line_start = context['line_start']
        self.line_end = context['line_end']
        self.column_start = context['column_start']
        self.column_end = context['column_end']

        self.selections = [Selection(x) for x in context['selections']]

class Parser(threading.Thread):
    def __init__(self, doc, finishedcb):
        self.doc = doc
        self.contexts = []

        self.finishedcb = finishedcb

        self.filename = None
        self.istmp = False

        if doc.get_modified() or doc.is_untitled():
            loc = doc.get_location()

            if loc:
                dirname = loc.get_parent()
                basename = loc.get_basename()

                f = dirname.get_child("." + basename + ".parse-tmp")
                fp = file(f.get_path(), 'w')
            else:
                fp = tempfile.NamedTemporaryFile(mode='w', delete=False)
                f = Gio.file_new_for_path(fp.name)

            bounds = doc.get_bounds()
            fp.write(doc.get_text(bounds[0], bounds[1], True))
            fp.close()

            self.filename = os.path.realpath(f.get_path())
            self.istmp = True
        else:
            self.filename = doc.get_location().get_path()

    def cancel(self):
        self.finishedcb = None

    def run(self):
        try:
            dirname = os.path.dirname(self.filename)
            filename = os.path.basename(self.filename)

            p = subprocess.Popen(['cpg-context', filename], cwd=dirname, stdout=subprocess.PIPE)

            self.ret = json.load(p.stdout)
        except Exception as (e):
            self.ret = None

        if self.ret and self.ret['status'] == 'ok':
            self.ret['data'] = filter(lambda x: x['filename'] == self.filename, self.ret['data'])

        if self.istmp:
            try:
                os.unlink(self.filename)
            except:
                pass

        GObject.idle_add(self.idle_finished)

    def idle_finished(self):
        if self.finishedcb:
            self.finishedcb(self)

        return False

# vi:ex:ts=4:et
