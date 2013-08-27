# -*- coding: utf-8 -*-
#
#  selections.py - cdncontext gedit plugin
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

from gi.repository import Gtk

class Selections(Gtk.TreeStore):
    def __init__(self):
        super(Gtk.TreeStore, self).__init__()

        self.set_column_types(('gchararray', 'gboolean'))
        self.context = None

    def update(self, context):
        if self.context == context:
            return

        self.clear()
        self.context = context

        if not self.context:
            return

        for selection in context.selections:
            self.add_selection(selection)

    def add_selection(self, selection):
        piter = self.append(None, ('[<b>in</b>]', False))

        for item in selection.ins:
            self.add_selection_item(piter, item)

        piter = self.append(None, ('[<b>out</b>]', False))

        for item in selection.outs:
            self.add_selection_item(piter, item)

    def add_selection_item(self, parent, selection):
        piter = self.append(parent, ("[<b>%s</b>] %s" % (selection.typename, selection.name), True))

        cnt = 1

        for expansion in selection.expansions:
            s = ', '.join(["%s<small><span color=\"#aaa\">(<i>%s</i>)</span></small>" % (x.value, x.index) for x in expansion.items])

            self.append(piter, ("%d. [%s]" % (cnt, s), False))
            cnt += 1

        if not selection.defines:
            return

        self.append(piter, ('<b>Defines</b>', True))

        for define in selection.defines:
            self.append(piter, ('%s: %s' % (define.key, define.value), False))

# vi:ex:ts=4:et
