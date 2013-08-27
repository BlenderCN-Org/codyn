# -*- coding: utf-8 -*-
#
#  windowactivatable.py - cdncontext gedit plugin
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

from panel import Panel
from shareddata import SharedData

from gi.repository import GObject, Gedit, Gtk

class WindowActivatable(GObject.Object, Gedit.WindowActivatable):
    window = GObject.property(type=Gedit.Window)

    def __init__(self):
        GObject.Object.__init__(self)

    def do_activate(self):
        self.panel = Panel()
        self.panel.show()

        side = self.window.get_side_panel()

        side.add_item_with_stock_icon(self.panel, "cdncontext", "CDN Context", Gtk.STOCK_INDEX)

        SharedData().panel = self.panel

    def do_deactivate(self):
        side = self.window.get_side_panel()
        side.remove_item(self.panel)

        self.window = None

# vi:ex:ts=4:et
