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

class Window:
    def __init__(self, n):
        self.n = n

        self.history = []
        self.history_ptr = -1

        Gtk.AccelMap.add_entry("<cdn-inspect>/Global/HistoryBack",
                               Gdk.KEY_Left,
                               Gdk.ModifierType.CONTROL_MASK)

        Gtk.AccelMap.add_entry("<cdn-inspect>/Global/HistoryForward",
                               Gdk.KEY_Right,
                               Gdk.ModifierType.CONTROL_MASK)

        Gtk.AccelMap.add_entry("<cdn-inspect>/Global/Reload",
                               Gdk.KEY_R,
                               Gdk.ModifierType.CONTROL_MASK)

        grp = Gtk.AccelGroup()
        grp.connect_by_path("<cdn-inspect>/Global/HistoryBack", self.on_history_back)
        grp.connect_by_path("<cdn-inspect>/Global/HistoryForward", self.on_history_forward)
        grp.connect_by_path("<cdn-inspect>/Global/Reload", self.on_reload)

        self.build_ui()

        self['window'].add_accel_group(grp)

        self.reload()

    def __getitem__(self, name):
        return self.builder.get_object(name)

    def on_reload(self, *args):
        self.reload()

    def reload(self):
        f = self.n.get_file()
        self.n = Cdn.Network.new_from_file(f)

        self['treestore'].clear()
        self.mapping = {}
        self.history = []
        self.history_ptr = -1

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

                self['treeview'].expand_to_path(path)
                self['treeview'].get_selection().select_path(path)

                # Highlight thingie
                it = self['treestore'].get_iter(path)
                e = self['treestore'].get_value(it, Column.EXPRESSION_ORIG)

                expr = err.get_expression()
                start = expr.get_error_start()
                off = expr.get_error_at()

                prefix = GLib.markup_escape_text(e[0:(start - 1)])
                suffix = GLib.markup_escape_text(e[(off - 1):])
                infix = GLib.markup_escape_text(e[(start - 1):(off - 1)])

                newe = prefix + '<span underline="error">' + infix + '</span>' + suffix

                self['treestore'].set(it, Column.EXPRESSION, newe)

        self.update_sensitivity()

    def on_history_forward(self, accelgroup, window, key, flags):
        self.on_forward_clicked()
        return True

    def on_history_back(self, accelgroup, window, key, flags):
        self.on_back_clicked()
        return True

    def update_sensitivity(self):
        self['toolbutton_back'].set_sensitive(self.history_ptr > 0)
        self['toolbutton_forward'].set_sensitive(self.history_ptr < len(self.history) - 1)

    def build_ui(self):
        self.icons = {}

        for name in ('variable', 'object', 'function', 'edge', 'action', 'blank'):
            try:
                self.icons[name] = GdkPixbuf.Pixbuf.new_from_file(os.path.join(root, name + '.png'))
            except:
                pass

        self.builder = Gtk.Builder()
        self.builder.add_from_file(os.path.join(root, 'window.ui'))

        self['window'].set_default_size(600, 800)

        self['toolbutton_back'].connect('clicked', self.on_back_clicked)
        self['toolbutton_forward'].connect('clicked', self.on_forward_clicked)

        self['toolbar'].get_style_context().add_class("primary-toolbar")

        tveq = self['textview_equation']

        settings = Gio.Settings('org.gnome.desktop.interface')
        font = settings.get_string('monospace-font-name')

        tveq.override_font(Pango.FontDescription(font))

        for l in ['info', 'sparsity', 'dimension']:
            self['label_' + l].override_font(Pango.FontDescription(font))

        tveq.connect('motion-notify-event', self.on_eq_motion)
        tveq.connect('button-press-event', self.on_eq_button_press)

        css = Gtk.CssProvider()
        css.load_from_data('GtkTextView { background-color: @theme_bg_color; }')
        tveq.get_style_context().add_provider(css, 600)

        self.vtag = tveq.get_buffer().create_tag('v', foreground="#ce5c00")

        self['treeview'].get_selection().connect('changed', self.on_selection_changed)

        self['button_simplify'].connect('clicked', self.on_simplify)

        self.update_sensitivity()

    def from_history(self):
        self['treeview'].get_selection().select_path(self.history[self.history_ptr])
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

        if hasattr(eq, 'get_variable'):
            v = eq.get_variable()
        elif hasattr(eq, 'get_function'):
            v = eq.get_function()

        if v in self.mapping:
            path = self.mapping[v]

            self['treeview'].expand_to_path(path)
            self['treeview'].scroll_to_cell(path, None, True, 0.5, 0)
            self['treeview'].get_selection().select_path(path)

    def get_eq_ref(self, ev):
        x, y = self['textview_equation'].window_to_buffer_coords(Gtk.TextWindowType.TEXT, ev.x, ev.y)

        it, trailing = self['textview_equation'].get_iter_at_position(x, y)
        location = self['textview_equation'].get_iter_location(it)

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

    def on_selection_changed(self, sel):
        model, it = sel.get_selected()

        buf = self['textview_equation'].get_buffer()

        for l in ['dimension', 'sparsity', 'info']:
            self['label_' + l].set_text('')

        buf.set_text('')

        if not it:
            return

        obj = model.get_value(it, Column.OBJECT)

        if isinstance(obj, Cdn.Annotatable):
            an = obj.get_annotation()

            if not an:
                an = ""

            lines = an.splitlines()
            retan = ""

            for line in lines:
                retan += line

                if line.endswith('.'):
                    retan += "\n"

            self['label_info'].set_markup(retan.rstrip("\n"))

        e = self.get_expression(obj)

        self.eqrefs = []

        if e:
            #eit = Cdn.ExpressionTreeIter.new(e)
            eits = e.get_as_string()
            instr = e.get_instructions()

            buf.set_text(eits)

            for i in instr:
                v = i.as_variable()
                c = i.as_custom_function()

                if v or c:
                    s, et = i.get_location()

                    if s == 0 or et == 0:
                        continue

                    start = buf.get_start_iter()
                    start.set_line_index(s - 1)

                    end = buf.get_start_iter()
                    end.set_line_index(et - 1)

                    buf.apply_tag(self.vtag, start, end)
                    self.eqrefs.append((s - 1, et - 1, v or c))

            hasdim, dim = e.get_dimension()

            if hasdim:
                self['label_dimension'].set_text('[%d-by-%d]: ' % (dim.rows, dim.columns))

            arg = e.get_stack_arg()
            sparse = [str(x) for x in arg.get_sparsity()]

            self['label_sparsity'].set_text('[' + ', '.join(sparse) + ']')

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

            vv = self['treestore'].append(parent)
            path = self['treestore'].get_path(vv)

            self.mapping[v] = path
            self.mapping[self.get_expression(v)] = path

            e = self.get_expression_s(v)

            self['treestore'].set(vv,
                           Column.OBJECT, v,
                           Column.NAME, v.get_name(),
                           Column.EXPRESSION, GLib.markup_escape_text(e),
                           Column.EXPRESSION_ORIG, e,
                           Column.ICON, self.get_icon(v))

        if isinstance(obj, Cdn.Node):
            for c in obj.get_children():
                par = self['treestore'].append(parent)
                self.mapping[c] = self['treestore'].get_path(par)

                e = self.get_expression_s(c)

                self['treestore'].set(par,
                               Column.OBJECT, c,
                               Column.NAME, c.get_id(),
                               Column.ICON, self.get_icon(c),
                               Column.EXPRESSION, GLib.markup_escape_text(e),
                               Column.EXPRESSION_ORIG, e)

                self.add_to_tree(par, c)
        elif isinstance(obj, Cdn.Edge):
            for c in obj.get_actions():
                par = self['treestore'].append(parent)

                path = self['treestore'].get_path(par)

                self.mapping[c] = path
                self.mapping[self.get_expression(c)] = path

                e = self.get_expression_s(c)

                target = c.get_target()
                indices = c.get_indices()

                if len(indices) > 0:
                    target += "[" + ', '.join([str(i) for i in indices]) + ']'

                self['treestore'].set(par,
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
            win['window'].show()

            app.add_window(win['window'])

app = Application()
app.run(sys.argv)

# vi:ts=4:et

