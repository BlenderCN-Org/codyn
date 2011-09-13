# -*- coding: utf-8 -*-
#
#  baseviewactivatable.py - cpgcontext gedit plugin
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

from shareddata import SharedData
import parser
import utils

class BaseViewActivatable:
    def __init__(self):
        self.timeout = None
        self.cursor_timeout = None
        self.parser = None
        self.iscpg = False
        self.last_parsed = []
        self.active_handlers = []
        self.handlers = []
        self.last_error = None

    def do_activate(self):
        doc = self.view.get_buffer()

        self.handlers = [
            doc.connect('notify::language', self.on_language_changed)
        ]

        self.error_tag = doc.create_tag('CpgParsed::error')
        doc.get_tag_table().remove(self.error_tag)
        self.error_tag.props.underline = utils.Underline.ERROR

        self.language_changed()

    def on_query_tooltip(self, view, x, y, keyboard_mode, tooltip):
        if not self.last_error:
            return False

        xy = self.view.window_to_buffer_coords(utils.TextWindowType.WIDGET, x, y)

        if not xy:
            return False

        piter = self.view.get_iter_at_location(xy[0], xy[1])

        if not piter:
            return False

        if not piter.has_tag(self.error_tag):
            return False

        tooltip.set_text(self.last_error.message)
        return True

    def cancel(self):
        doc = self.view.get_buffer()

        if not self.iscpg:
            return

        doc.get_tag_table().remove(self.error_tag)

        for x in self.active_handlers:
            x[0].disconnect(x[1])

        self.active_handlers = []

        if self.parser:
            self.parser.cancel()
            self.parser = None

        if self.timeout:
            utils.source_remove(self.timeout)
            self.timeout = None

        if self.cursor_timeout:
            utils.source_remove(self.cursor_timeout)
            self.cursor_timeout = None

    def language_changed(self):
        doc = self.view.get_buffer()
        lang = doc.get_language()

        iscpg = (lang and lang.get_id() == 'cpg')

        if iscpg == self.iscpg:
            return

        self.cancel()
        self.iscpg = iscpg

        if self.iscpg:
            self.active_handlers = [
                [doc, doc.connect('changed', self.on_doc_changed)],
                [doc, doc.connect('saved', self.on_doc_saved)],
                [doc, doc.connect('cursor-moved', self.on_cursor_moved)],
                [self.view, self.view.connect('query-tooltip', self.on_query_tooltip)]
            ]

            doc.get_tag_table().add(self.error_tag)

            self.reparse()

    def on_language_changed(self, doc, spec):
        self.language_changed()

    def on_doc_saved(self, doc, error):
        self.reparse()

    def reparse(self):
        if self.timeout:
            utils.source_remove(self.timeout)
            self.timeout = None

        self.timeout = utils.timeout_add(200, self.on_doc_parse_timeout)

    def on_doc_changed(self, doc):
        self.reparse()

    def clear_parsed(self):
        doc = self.view.get_buffer()
        bounds = doc.get_bounds()

        doc.remove_tag(self.error_tag, bounds[0], bounds[1])

    def update_parsed_error(self, data):
        self.last_parsed = []
        self.last_error = data

        message = data.message
        lstart = data.line_start
        lend = data.line_end
        cstart = data.column_start
        cend = data.column_end

        doc = self.view.get_buffer()
        piter = doc.get_iter_at_line(max(0, lstart - 1))

        if piter:
            pend = piter.copy()

            piter.forward_chars(max(cstart - 1, 0))

            if lend > lstart:
                pend.forward_lines(lend - lstart)

            pend.forward_chars(max(cend, 0))

            doc.apply_tag(self.error_tag, piter, pend)

    def find_context_at_cursor(self):
        if not self.last_parsed:
            return None

        doc = self.view.get_buffer()
        piter = doc.get_iter_at_mark(doc.get_insert())

        line = piter.get_line() + 1
        col = piter.get_line_offset() + 1

        # Find the last context containing the cursor location
        lastmatch = None

        for context in self.last_parsed:
            if context.line_start > line:
                return lastmatch

            if context.line_end < line:
                continue

            # Start line condition
            if line == context.line_start and col <= context.column_start:
                continue

            # End line condition
            if line == context.line_end and col > context.column_end:
                continue

            lastmatch = context

        return lastmatch

    def timeout_cursor_moved(self):
        self.cursor_timeout = None

        context = self.find_context_at_cursor()

        SharedData().panel.model.update(context)
        SharedData().panel.treeview.expand_all()

    def cursor_moved(self):
        if self.cursor_timeout:
            utils.source_remove(self.cursor_timeout)
            self.cursor_timeout = None

        self.cursor_timeout = utils.timeout_add(100, self.timeout_cursor_moved)

    def on_cursor_moved(self, doc):
        self.cursor_moved()

    def update_parsed_ok(self, data):
        self.last_parsed = data
        self.last_error = None

        self.cursor_moved()

    def update_parsed(self, ret):
        self.clear_parsed()

        if not ret:
            return

        if ret['status'] == 'error':
            self.update_parsed_error(ret['data'])
        else:
            self.update_parsed_ok(ret['data'])

    def on_parser_finished(self, ret):
        self.parser = None

        self.update_parsed(ret)

    def on_doc_parse_timeout(self):
        self.timeout = None

        if self.parser != None:
            self.parser.cancel()
            self.parser = None

        self.parser = parser.Parser(self.view.get_buffer(), self.on_parser_finished)
        self.parser.run()

        return False

    def do_deactivate(self):
        doc = self.view.get_buffer()

        for handler in self.handlers:
            doc.disconnect(handler)

        self.handlers = []

        self.cancel()

        self.view = None

# vi:ex:ts=4:et
