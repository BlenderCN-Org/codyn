# -*- coding: utf-8 -*-
#
#  basewindowactivatable.py - cdncontext gedit plugin
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
import utils

from shareddata import SharedData

class BaseWindowActivatable:
    def __init__(self):
        pass

    def do_activate(self):
        self.panel = Panel()
        self.panel.show()

        side = self.window.get_side_panel()

        if utils.isgi:
            side.add_item_with_stock_icon(self.panel, "cdncontext", "CDN Context", utils.gtk.STOCK_INDEX)
        else:
            side.add_item(self.panel, "Cdn Context", utils.gtk.STOCK_INDEX)

        SharedData().panel = self.panel

    def do_deactivate(self):
        side = self.window.get_side_panel()
        side.remove_item(self.panel)

        self.window = None

# vi:ex:ts=4:et
