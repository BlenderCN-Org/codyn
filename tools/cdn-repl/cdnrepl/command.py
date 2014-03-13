import cmd, readline, os, sys, glob

from gi.repository import Cdn

class Command(cmd.Cmd):
    prompt = '>>> '
    intro = """Welcome to the codyn 3.0 REPL.
Type `help' for more information."""

    def __init__(self, *args):
        cmd.Cmd.__init__(self, *args)

        self._setup_readline()
        self.network = Cdn.Network()
        self.filename = None

        self._selections = None
        self._set_selections()

    def cmdloop(self, intro=None):
        while True:
            try:
                cmd.Cmd.cmdloop(self, intro)
            except KeyboardInterrupt:
                self.intro = None

                if not self._rl_buffer is None and len(self._rl_buffer.value) > 0:
                    self._rl_buffer.value = self._new_empty_cstr()
                    self.stdout.write('\n')
                else:
                    self.stdout.write('^C\n')
                    sys.exit(1)

    def _cache_dir(self):
        cache = os.getenv('XDG_CACHE_HOME')

        if cache is None:
            cache = os.path.join(os.path.expanduser('~'), '.cache')

        return os.path.join(cache, 'cdn-repl')

    def _wrap_native(self):
        import ctypes

        self._rl_buffer = None

        if 'libedit' in readline.__doc__:
            libname = 'libedit.dylib'
        else:
            libname = 'libreadline.so'

        try:
            libreadline = ctypes.CDLL(libname)
        except:
            return

        self._rl_buffer = ctypes.c_char_p.in_dll(libreadline, 'rl_line_buffer')

        libme = ctypes.CDLL(None)
        libme.calloc.argtypes = [ctypes.c_size_t, ctypes.c_size_t]
        libme.calloc.restype = ctypes.c_char_p

        def el():
            return libme.calloc(1, 1)

        self._new_empty_cstr = el

        self._rl_filename_completion_function = libreadline.rl_filename_completion_function
        self._rl_filename_completion_function.restype = ctypes.c_char_p
        self._rl_filename_completion_function.argtypes = [ctypes.c_char_p, ctypes.c_int]

    def _setup_readline(self):
        self._wrap_native()

        cd = self._cache_dir()

        try:
            os.makedirs(cd)
        except OSError:
            pass

        historyfile = os.path.join(cd, 'history')

        try:
            readline.read_history_file(historyfile)
        except IOError:
            pass

        import atexit
        atexit.register(readline.write_history_file, historyfile)

        if 'libedit' in readline.__doc__:
            self.completekey = None

            readline.set_completer(self.complete)
            readline.parse_and_bind('bind ^I rl_complete')

    def _update_prompt(self):
        self.prompt = '({0}) >>> '.format(', '.join([x.get_object() == self.network and 'cdn' or x.get_object().get_full_id_for_display() for x in self._selections]))

    def _set_selections(self, selections=None):
        self._prev_selections = self._selections

        if self._prev_selections is None:
            self._prev_selections = [Cdn.Selection.new(self.network, None)]

        if selections is None or len(selections) == 0:
            self._selections = [Cdn.Selection.new(self.network, None)]
        else:
            self._selections = selections

        self._update_prompt()

    def _complete_filename(self, text):
        ret = []
        state = 0

        while True:
            n = self._rl_filename_completion_function(text, state)

            if n is None:
                break

            state += 1
            ret.append(n)

        return ret

    def do_d(self, s):
        self.do_display(s)

    def do_display(self, s):
        s = s.replace('..', ' | parent ')

        try:
            selector = Cdn.Selector.parse(self.network, s)
        except Exception as e:
            self.stdout.write('Failed to parse selector: {0}\n'.format(e.message))
            return

        sel = selector.select_set(self._selections, Cdn.SelectorType.OBJECT | Cdn.SelectorType.VARIABLE)

        objs = filter(lambda x: isinstance(x, Cdn.Object), [x.get_object() for x in sel])
        vars = filter(lambda x: isinstance(x, Cdn.Variable), [x.get_object() for x in sel])

        if len(objs) == 0 and len(vars) == 0:
            return

        self.stdout.write('\n')

        isone = len(vars) == 1

        for v in vars:
            indent = ''

            if not isone:
                self.stdout.write('{0}:\n'.format(v.get_full_name_for_display()))
                indent = '    '

            vals = v.get_values()
            dim = vals.dimension()

            if dim.is_one():
                self.stdout.write('{0}{1}\n'.format(indent, vals[0]))
            else:
                self.stdout.write('{0}['.format(indent))

                for r in range(0, dim.rows):
                    if r != 0:
                        self.stdout.write(';\n{0} '.format(indent))

                    for c in range(0, dim.columns):
                        if c != 0:
                            self.stdout.write(', ')

                        val = vals.get_at(r, c)

                        self.stdout.write('{0: 9.4f}'.format(val))

                self.stdout.write('] ({0}-by-{1})\n'.format(dim.rows, dim.columns))

            self.stdout.write('\n')

    def do_ls(self, s):
        objects = filter(lambda x: isinstance(x, Cdn.Object), [x.get_object() for x in self._selections])
        isone = len(objects) == 1

        for o in objects:
            if not isone:
                sys.stdout.write('[{0}]\n'.format(o.get_full_id_for_display()))

            objs = [c.get_id() for c in o.get_children()]
            vars = ['(' + v.get_name() + ')' for v in o.get_variables()]

            self.columnize(objs + vars)

    def do_load(self, s):
        self.load_network(s)

    def load_network(self, s):
        n = Cdn.Network()

        try:
            n.load_from_path(s)
        except Exception as e:
            self.stdout.write('Error loading network: {0}\n'.format(e.message))
            return False

        err = Cdn.CompileError()

        if not n.compile(None, err):
            self.stdout.write('Error compiling network: {0}\n'.format(err.get_formatted_string()))
            return False

        self.filename = s
        self.network = n

        self._selections = None
        self._set_selections()

        return True

    def complete_load(self, text, line, begidx, endidx):
        return self._complete_filename(text)

    def do_r(self, s):
        self.do_reload(s)

    def do_reload(self, s):
        if self.filename is None:
            self.stdout.write('No network has been loaded yet\n')
            return

        objs = filter(lambda x: isinstance(x, Cdn.Object), [x.get_object() for x in self._selections])

        if self.load_network(self.filename):
            newsel = []

            for obj in objs:
                if isinstance(obj, Cdn.Network):
                    obj = self.network
                else:
                    obj = self.network.find_object(obj.get_full_id())

                if not obj is None:
                    newsel.append(Cdn.Selection.new(obj, None))

            self._set_selections(newsel)

    def do_cd(self, s):
        s = s.strip()

        if len(s) == 0:
            self._set_selections()
            return
        elif s == '-':
            self._set_selections(list(self._prev_selections))
            return

        nochild = s.startswith('..')

        if s.endswith('..'):
            s = s[:-2] + ' | parent'

        s = s.replace('..', ' | parent .')

        try:
            selector = Cdn.Selector.parse(self.network, s)
            selector.set_implicit_children(not nochild)
        except Exception as e:
            self.stdout.write('Failed to parse selector: {0}\n'.format(e.message))
            return

        sel = selector.select_set(self._selections, Cdn.SelectorType.OBJECT)

        if len(sel) == 0:
            self.stdout.write('Did not find any nodes matching this selector\n')
        else:
            self._set_selections(sel)

    def do_EOF(self, s):
        sys.exit(1)

# vi:ts=4:et
