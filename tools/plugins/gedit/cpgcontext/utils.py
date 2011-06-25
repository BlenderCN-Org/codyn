# -*- coding: utf-8 -*-
#
#  utils.py - cpgcontext gedit plugin
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

try:
    from gi.repository import GObject, Gtk, Pango

    isgi = True

    gtk = Gtk
    pango = Pango
except:
    import gobject, gtk, pango

    isgi = False

if isgi:
    def io_add_watch(fp, cb):
        GObject.io_add_watch(fp, GObject.IO_IN | GObject.IO_HUP, cb)

    def io_is_in(condition):
        return condition & (GObject.IO_IN | GObject.IO_PRI)

    def io_is_close(condition):
        return condition & ~(GObject.IO_IN | GObject.IO_PRI)

    def get_style_fg(widget, state):
        return widget.get_style_context().get_color(state)

    def get_style_bg(widget, state):
        return widget.get_style_context().get_background_color(state)

    idle_add = GObject.idle_add
    timeout_add = GObject.timeout_add
    source_remove = GObject.source_remove
    child_watch_add = GObject.child_watch_add

    PolicyType = Gtk.PolicyType
    EllipsizeMode = Pango.EllipsizeMode
    StateFlags = Gtk.StateFlags
    Underline = Pango.Underline
    TextWindowType = Gtk.TextWindowType
    IconSize = Gtk.IconSize
    ShadowType = Gtk.ShadowType

else:
    def io_add_watch(fp, cb):
        gobject.io_add_watch(fp, gobject.IO_IN | gobject.IO_HUP, cb)

    def io_is_in(condition):
        return condition & (gobject.IO_IN | gobject.IO_PRI)

    def io_is_close(condition):
        return condition & ~(gobject.IO_IN | gobject.IO_PRI)

    def get_style_fg(widget, state):
        return widget.get_style().fg[state]

    def get_style_bg(widget, state):
        return widget.get_style().bg[state]

    idle_add = gobject.idle_add
    timeout_add = gobject.timeout_add
    source_remove = gobject.source_remove
    child_watch_add = gobject.child_watch_add

    class PolicyType:
        AUTOMATIC = gtk.POLICY_AUTOMATIC

    class EllipsizeMode:
        END = pango.ELLIPSIZE_END

    class StateFlags:
        ACTIVE = gtk.STATE_ACTIVE
        NORMAL = gtk.STATE_NORMAL
        INSENSITIVE = gtk.STATE_INSENSITIVE
        PRELIGHT = gtk.STATE_PRELIGHT

    class Underline:
        ERROR = pango.UNDERLINE_ERROR

    class TextWindowType:
        WIDGET = gtk.TEXT_WINDOW_WIDGET

    class IconSize:
        MENU = gtk.ICON_SIZE_MENU
        BUTTON = gtk.ICON_SIZE_BUTTON

    class ShadowType:
        ETCHED_IN = gtk.SHADOW_ETCHED_IN

# vi:ex:ts=4:et
