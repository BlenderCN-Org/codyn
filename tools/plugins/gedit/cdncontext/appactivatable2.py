# -*- coding: utf-8 -*-
#
#  appactivatable.py - cdncontext gedit plugin
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

import gedit

from windowactivatable import WindowActivatable

class Plugin(gedit.Plugin):
    def __init__(self):
        self.helpers = {}

    def activate(self, window):
        self.helpers[window] = WindowActivatable(window)
        self.helpers[window].activate()

    def deactivate(self, window):
        if window in self.helpers:
            self.helpers[window].deactivate()
            del self.helpers[window]

# vi:ex:ts=4:et
