# -*- coding: utf-8 -*-
#
#  panel.py - cdncontext gedit plugin
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

from selections import Selections
from gi.repository import Gtk, Pango

class Panel(Gtk.Box):
    def __init__(self):
        Gtk.Box.__init__(self)

        self.set_orientation(Gtk.Orientation.VERTICAL)

        sw = Gtk.ScrolledWindow()
        sw.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        sw.set_shadow_type(Gtk.ShadowType.ETCHED_IN)
        sw.show()

        self.treeview = Gtk.TreeView()
        self.model = Selections()

        self.treeview.set_model(self.model)
        self.treeview.show()
        self.treeview.set_headers_visible(False)
        self.treeview.set_show_expanders(False)
        self.treeview.set_level_indentation(12)

        cell = Gtk.CellRendererText()
        cell.props.ellipsize = Pango.EllipsizeMode.END

        column = Gtk.TreeViewColumn()
        column.props.title = 'Context'

        column.pack_start(cell, True)
        column.add_attribute(cell, 'markup', 0)

        column.set_cell_data_func(cell, self.on_cell_data)

        self.treeview.append_column(column)

        sw.add(self.treeview)

        self.pack_start(sw, True, True, 0)

    def on_cell_data(self, column, renderer, model, piter, data=None):
        if model.get_value(piter, 1):
            if not model.iter_parent(piter):
                state = Gtk.StateFlags.ACTIVE
            else:
                state = Gtk.StateFlags.INSENSITIVE

            ctx = self.treeview.get_style_context()

            bg = ctx.get_background_color(state)
            fg = ctx.get_color(state)

            renderer.props.background_rgba = bg
            renderer.props.foreground_rgba = fg
        else:
            renderer.props.background_set = False
            renderer.props.foreground_set = False

# vi:ex:ts=4:et
