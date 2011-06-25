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

from basewindowactivatable import BaseWindowActivatable
from viewactivatable import ViewActivatable

class WindowActivatable(BaseWindowActivatable):
    def __init__(self, window):
        self.window = window

        self._helpers = {}
        self._signals = []

    def activate(self):
        self.do_activate()

        self._signals = [
            self.window.connect('tab-added', self.on_tab_added),
            self.window.connect('tab-removed', self.on_tab_removed)
        ]

    def deactivate(self):
        for s in self._signals:
            self.window.disconnect(s)

        self._signals = []
        self.do_deactivate()

        for k in self._helpers:
            self._helpers[k].do_deactivate()

        self._helpers = {}

    def on_tab_added(self, window, tab):
        view = tab.get_view()
        helper = ViewActivatable(view)

        self._helpers[view] = helper
        helper.do_activate()

    def on_tab_removed(self, window, tab):
        view = tab.get_view()

        if view in self._helpers:
            self._helpers[view].do_deactivate()
            del self._helpers[view]

# vi:ex:ts=4:et
