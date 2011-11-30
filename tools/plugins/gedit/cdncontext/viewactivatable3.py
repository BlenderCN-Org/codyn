# -*- coding: utf-8 -*-
#
#  viewactivatable.py - cdncontext gedit plugin
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

from gi.repository import GObject, Gedit, Pango, Gtk
from baseviewactivatable import BaseViewActivatable

class ViewActivatable(BaseViewActivatable, GObject.Object, Gedit.ViewActivatable):
    view = GObject.property(type=Gedit.View)

    def __init__(self):
        GObject.Object.__init__(self)
        BaseViewActivatable.__init__(self)

    def do_activate(self):
        BaseViewActivatable.do_activate(self)

    def do_deactivate(self):
        BaseViewActivatable.do_deactivate(self)

# vi:ex:ts=4:et
