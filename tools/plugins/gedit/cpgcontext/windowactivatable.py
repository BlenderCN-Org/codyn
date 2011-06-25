# -*- coding: utf-8 -*-
#
#  windowactivatable.py - cpgcontext gedit plugin
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

from gi.repository import GObject, Gedit, Gtk
from panel import Panel
from shareddata import SharedData

class WindowActivatable(GObject.Object, Gedit.WindowActivatable):
    window = GObject.property(type=Gedit.Window)

    def __init__(self):
        GObject.Object.__init__(self)

    def do_activate(self):
        panel = Panel()
        panel.show()

        side = self.window.get_side_panel()
        side.add_item_with_stock_icon(panel, "cpgcontext", "CPG Context", Gtk.STOCK_INDEX)

        SharedData().panel = panel

    def do_deactivate(self):
        pass

# vi:ex:ts=4:et
