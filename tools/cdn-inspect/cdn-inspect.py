#!/usr/bin/python

import sys, signal, os, re

signal.signal(signal.SIGINT, signal.SIG_DFL)

from gi.repository import Cdn, Gio, Gtk, GLib, Gdk, GdkPixbuf, Pango

global root

root = os.path.dirname(__file__)

class Column:
    OBJECT = 0
    NAME = 1
    EXPRESSION = 2
    ICON = 3
    EXPRESSION_ORIG = 4

class Window(Gtk.Window):
    def __init__(self, n):
        super(Gtk.Window, self).__init__()

        self.n = n
        self.set_default_size(800, 600)

        self.history = []
        self.history_ptr = -1

        Gtk.AccelMap.add_entry("<cdn-inspect>/Global/HistoryBack",
                               Gdk.KEY_Left,
                               Gdk.ModifierType.CONTROL_MASK)

        Gtk.AccelMap.add_entry("<cdn-inspect>/Global/HistoryForward",
                               Gdk.KEY_Right,
                               Gdk.ModifierType.CONTROL_MASK)

        grp = Gtk.AccelGroup()
        grp.connect_by_path("<cdn-inspect>/Global/HistoryBack", self.on_history_back)
        grp.connect_by_path("<cdn-inspect>/Global/HistoryForward", self.on_history_forward)

        self.add_accel_group(grp)

        self.build_ui()

        err = Cdn.CompileError()

        compiled = self.n.compile(None, err)

        self.add_to_tree(None, self.n)

        if not compiled:
            info = Gtk.InfoBar()
            info.add_button("Close", Gtk.ResponseType.CLOSE)
            info.set_message_type(Gtk.MessageType.ERROR)
            content = info.get_content_area()

            label = Gtk.Label(err.get_formatted_string())
            label.show()
            content.add(label)

            info.show()
            info.connect('response', lambda x, y: info.destroy())
            self.vbox.pack_start(info, False, False, 0)

            o = err.get_variable()

            if not o:
                o = err.get_edge_action()

            if not o:
                o = err.get_object()

            if o in self.mapping:
                path = self.mapping[o]

                self.tv.expand_to_path(path)
                self.tv.get_selection().select_path(path)

                # Highlight thingie
                it = self.model.get_iter(path)
                e = self.model.get_value(it, Column.EXPRESSION_ORIG)

                expr = err.get_expression()
                start = expr.get_error_start()
                off = expr.get_error_at()

                prefix = GLib.markup_escape_text(e[0:(start - 1)])
                suffix = GLib.markup_escape_text(e[(off - 1):])
                infix = GLib.markup_escape_text(e[(start - 1):(off - 1)])

                newe = prefix + '<span underline="error">' + infix + '</span>' + suffix

                self.model.set(it, Column.EXPRESSION, newe)

    def on_history_forward(self, accelgroup, window, key, flags):
        self.on_forward_clicked()
        return True

    def on_history_back(self, accelgroup, window, key, flags):
        self.on_back_clicked()
        return True

    def update_sensitivity(self):
        self.back.set_sensitive(self.history_ptr > 0)
        self.forward.set_sensitive(self.history_ptr < len(self.history) - 1)

    def build_ui(self):
        self.vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)
        self.vbox.show()

        tbar = Gtk.Toolbar()
        tbar.show()

        self.back = Gtk.ToolButton.new_from_stock(Gtk.STOCK_GO_BACK)
        self.back.show()
        self.back.set_is_important(True)
        self.back.connect('clicked', self.on_back_clicked)
        tbar.insert(self.back, -1)

        self.forward = Gtk.ToolButton.new_from_stock(Gtk.STOCK_GO_FORWARD)
        self.forward.show()
        self.forward.set_is_important(True)
        self.forward.connect('clicked', self.on_forward_clicked)
        tbar.insert(self.forward, -1)

        tbar.get_style_context().add_class("primary-toolbar")

        self.vbox.pack_start(tbar, False, True, 0)

        self.paned = Gtk.Paned(orientation=Gtk.Orientation.VERTICAL)
        self.paned.show()

        self.vbox.pack_end(self.paned, True, True, 0)

        self.icons = {}

        for name in ('variable', 'object', 'function', 'edge', 'action', 'blank'):
            try:
                self.icons[name] = GdkPixbuf.Pixbuf.new_from_file(os.path.join(root, name + '.png'))
            except:
                pass

        tv = Gtk.TreeView()
        tv.show()

        self.tv = tv

        col = Gtk.TreeViewColumn('Name')

        cell = Gtk.CellRendererPixbuf()
        cell.props.yalign = 0
        col.pack_start(cell, False)
        col.add_attribute(cell, "pixbuf", Column.ICON)

        cell = Gtk.CellRendererText()
        cell.props.yalign = 0
        col.pack_start(cell, False)
        col.add_attribute(cell, "text", Column.NAME)

        tv.append_column(col)

        cell = Gtk.CellRendererText()
        cell.props.font_desc = Pango.FontDescription('Monospace 8')
        cell.props.yalign = 0
        col = Gtk.TreeViewColumn('Value', cell, markup=Column.EXPRESSION)
        tv.append_column(col)

        sw = Gtk.ScrolledWindow()
        sw.show()
        sw.add(tv)
        sw.set_shadow_type(Gtk.ShadowType.ETCHED_IN)

        self.model = Gtk.TreeStore(object, str, str, GdkPixbuf.Pixbuf, str)
        tv.set_model(self.model)

        self.mapping = {}

        self.paned.pack1(sw, True, False)

        vb = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6, border_width=6)
        vb.show()

        hbox = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=6)
        hbox.show()

        but = Gtk.Button('Simplify')
        but.show()

        but.connect('clicked', self.on_simplify)
        hbox.pack_end(but, False, False, 0)

        grid = Gtk.Grid()
        grid.show()
        grid.set_column_spacing(6)
        grid.set_row_spacing(3)

        grid.attach(self.make_label('<b>Info:</b>'), 0, 0, 1, 1)

        self.info = Gtk.Label()
        self.info.set_line_wrap(True)
        self.info.set_hexpand(True)
        self.info.props.halign = Gtk.Align.START
        grid.attach(self.info, 1, 0, 1, 1)

        grid.attach(self.make_label('<b>Eq:</b>'), 0, 1, 1, 1)
        self.eq = Gtk.TextView()
        self.eq.set_editable(False)
        self.eq.set_cursor_visible(False)
        self.eq.set_hexpand(True)
        self.eq.show()

        self.eq.override_font(Pango.FontDescription('Monospace 12'))
        self.eqrefs = []

        self.eq.connect('motion-notify-event', self.on_eq_motion)
        self.eq.connect('button-press-event', self.on_eq_button_press)

        self.vtag = self.eq.get_buffer().create_tag('v', foreground="#ce5c00")

        grid.attach(self.eq, 1, 1, 1, 1)

        css = Gtk.CssProvider()
        css.load_from_data('GtkTextView { background-color: @theme_bg_color; }')
        self.eq.get_style_context().add_provider(css, 600)

        grid.show_all()

        vb.pack_start(grid, True, True, 0)
        vb.pack_start(hbox, False, True, 0)

        self.paned.pack2(vb, False, False)
        self.add(self.vbox)

        self.tv.get_selection().connect('changed', self.on_selection_changed)

        self.update_sensitivity()

    def from_history(self):
        self.tv.get_selection().select_path(self.history[self.history_ptr])
        self.update_sensitivity()

    def on_forward_clicked(self, *args):
        if self.history_ptr < len(self.history) - 1:
            self.history_ptr += 1
            self.from_history()

    def on_back_clicked(self, *args):
        if self.history_ptr > 0:
            self.history_ptr -= 1
            self.from_history()

    def on_eq_button_press(self, tv, ev):
        win = tv.get_window(Gtk.TextWindowType.TEXT)

        if ev.window != win:
            return False

        if ev.button != 1:
            return False

        eq = self.get_eq_ref(ev)

        if not eq:
            return False

        v = eq.get_variable()

        if v in self.mapping:
            path = self.mapping[v]

            self.tv.expand_to_path(path)
            self.tv.scroll_to_cell(path, None, True, 0.5, 0)
            self.tv.get_selection().select_path(path)

    def get_eq_ref(self, ev):
        x, y = self.eq.window_to_buffer_coords(Gtk.TextWindowType.TEXT, ev.x, ev.y)

        it, trailing = self.eq.get_iter_at_position(x, y)
        location = self.eq.get_iter_location(it)

        if x > location.x + location.width or \
           x < location.x or \
           y > location.y + location.height or \
           y < location.y:
            return None

        idx = it.get_line_index()

        for i in self.eqrefs:
            if idx >= i[0] and idx < i[1]:
                return i[2]

        return None

    def on_eq_motion(self, tv, ev):
        win = tv.get_window(Gtk.TextWindowType.TEXT)

        if ev.window != win:
            return False

        eq = self.get_eq_ref(ev)

        if eq:
            curs = Gdk.Cursor(Gdk.CursorType.HAND2)
            ev.window.set_cursor(curs)
            return False

        ev.window.set_cursor(None)
        return False

    def make_label(self, t):
        ret = Gtk.Label(t)
        ret.set_use_markup(True)
        ret.props.valign = Gtk.Align.START
        ret.show()

        return ret

    def on_selection_changed(self, sel):
        model, it = sel.get_selected()

        obj = model.get_value(it, Column.OBJECT)

        if isinstance(obj, Cdn.Annotatable):
            an = obj.get_annotation()

            if not an:
                an = ''
            else:
                an = re.sub(' *\n( +|(?=[^\n]))', ' ', an)

            self.info.set_markup(an)
        else:
            self.info.set_text('')

        e = self.get_expression(obj)
        buf = self.eq.get_buffer()

        self.eqrefs = []

        if e:
            #eit = Cdn.ExpressionTreeIter.new(e)

            eits = e.get_as_string()
            instr = e.get_instructions()

            buf.set_text(eits)

            for i in instr:
                v = i.as_variable()

                if v:
                    s, e = i.get_location()

                    start = buf.get_start_iter()
                    start.set_line_index(s - 1)

                    end = buf.get_start_iter()
                    end.set_line_index(e - 1)

                    buf.apply_tag(self.vtag, start, end)
                    self.eqrefs.append((s - 1, e - 1, v))
        else:
            buf.set_text('')

        self.add_history(model.get_path(it))

    def add_history(self, path):
        if self.history_ptr >= 0 and self.history_ptr < len(self.history) and \
            path == self.history[self.history_ptr]:
            return

        del self.history[self.history_ptr + 1:]
        self.history.append(path)
        self.history_ptr = len(self.history) - 1

        self.update_sensitivity()

    def on_simplify(self, but):
        self.n.simplify()

    def get_icon(self, o):
        name = 'blank'

        if isinstance(o, Cdn.Variable):
            name = 'variable'
        elif isinstance(o, Cdn.EdgeAction):
            name = 'action'
        elif isinstance(o, Cdn.Function):
            name = 'function'
        elif isinstance(o, Cdn.Edge):
            name = 'edge'
        elif isinstance(o, Cdn.Object):
            name = 'object'

        return self.icons[name]

    def get_expression(self, o):
        if hasattr(o, 'get_expression'):
            return o.get_expression()

        if hasattr(o, 'get_equation'):
            return o.get_equation()

        return None

    def get_expression_s(self, o):
        e = self.get_expression(o)

        if e:
            s = e.get_as_string()

            if len(s) > 30:
                s = s.replace('; ', ";\n")

            return s
        elif isinstance(o, Cdn.Edge):
            return '%s to %s' % (o.get_input().get_id(), o.get_output().get_id())
        else:
            return ''

    def add_to_tree(self, parent, obj):
        # Variables
        isfunc = isinstance(obj, Cdn.Function)

        for v in obj.get_variables():
            if isfunc and obj.get_argument(v.get_name()):
                continue

            vv = self.model.append(parent)
            path = self.model.get_path(vv)

            self.mapping[v] = path
            self.mapping[self.get_expression(v)] = path

            e = self.get_expression_s(v)

            self.model.set(vv,
                           Column.OBJECT, v,
                           Column.NAME, v.get_name(),
                           Column.EXPRESSION, GLib.markup_escape_text(e),
                           Column.EXPRESSION_ORIG, e,
                           Column.ICON, self.get_icon(v))

        if isinstance(obj, Cdn.Node):
            for c in obj.get_children():
                par = self.model.append(parent)
                self.mapping[c] = self.model.get_path(par)

                e = self.get_expression_s(c)

                self.model.set(par,
                               Column.OBJECT, c,
                               Column.NAME, c.get_id(),
                               Column.ICON, self.get_icon(c),
                               Column.EXPRESSION, GLib.markup_escape_text(e),
                               Column.EXPRESSION_ORIG, e)

                self.add_to_tree(par, c)
        elif isinstance(obj, Cdn.Edge):
            for c in obj.get_actions():
                par = self.model.append(parent)

                path = self.model.get_path(par)

                self.mapping[c] = path
                self.mapping[self.get_expression(c)] = path

                e = self.get_expression_s(c)

                target = c.get_target()
                indices = c.get_indices()

                if len(indices) > 0:
                    target += "[" + ', '.join([str(i) for i in indices]) + ']'

                self.model.set(par,
                               Column.OBJECT, c,
                               Column.NAME, target,
                               Column.ICON, self.get_icon(c),
                               Column.EXPRESSION, GLib.markup_escape_text(e),
                               Column.EXPRESSION_ORIG, e)

class Application(Gtk.Application):
    def __init__(self):
        super(Gtk.Application, self).__init__(application_id="net.codyn.Inspect",
                                              flags=Gio.ApplicationFlags.HANDLES_COMMAND_LINE)

    def do_command_line(self, cmd):
        ctx = GLib.OptionContext("cdn-inspect")

        args = cmd.get_arguments()[1:]
        ctx.parse(args)

        for arg in args:
            f = Gio.File.new_for_commandline_arg(arg)
            n = Cdn.Network.new_from_file(f)

            win = Window(n)
            win.show()

            app.add_window(win)

app = Application()
app.run(sys.argv)

# vi:ts=4:et

