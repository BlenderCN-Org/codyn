import cmd, readline, os, sys, glob, math, re

from gi.repository import Cdn

class Command(cmd.Cmd):
    prompt = '$ '
    intro = """Welcome to the codyn 3.0 REPL.
Type `help' for more information."""

    def __init__(self, *args):
        cmd.Cmd.__init__(self, *args)

        self._setup_readline()
        self.network = Cdn.Network()
        self.filename = None

        self._selections = None
        self._set_selections()
        self._watch = set()

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
        t = self.network.get_integrator().get_variable('t')
        self.prompt = '(\x1b[33m{0}\x1b[0m) [\x1b[36m{1}\x1b[0m]$ '.format(', '.join([x.get_object() == self.network and 'cdn' or x.get_object().get_full_id_for_display() for x in self._selections]), t.get_value())

    def _set_selections(self, selections=None):
        self._prev_selections = self._selections

        if self._prev_selections is None:
            self._prev_selections = [Cdn.Selection.new(self.network, None)]

        if selections is None or len(selections) == 0:
            self._selections = [Cdn.Selection.new(self.network, None)]
        else:
            objs = {}
            self._selections = []

            for s in selections:
                o = s.get_object()

                if not o in objs:
                    objs[o] = True
                    self._selections.append(s)

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

    def _display_vars(self, vars, alwaysname=False):
        if len(vars) == 0:
            return

        self.stdout.write('\n')

        isone = len(vars) == 1

        for v in vars:
            indent = ''

            if not isone or alwaysname:
                self.stdout.write('\x1b[32m{0}\x1b[0m:\n'.format(v.get_full_name_for_display()))
                indent = '    '

            vals = v.get_values()
            dim = vals.dimension()

            if dim.is_one():
                self.stdout.write('{0}{1}\n'.format(indent, vals.get_at(0, 0)))
            else:
                self.stdout.write('{0}['.format(indent))

                for r in range(0, dim.rows):
                    if r != 0:
                        self.stdout.write(';\n{0} '.format(indent))

                    for c in range(0, dim.columns):
                        if c != 0:
                            self.stdout.write(', ')

                        val = vals.get_at(r, c)

                        self.stdout.write('\x1b[35m{0: 9.4f}\x1b[0m'.format(val))

                self.stdout.write(' ] (\x1b[34m{0}-by-{1}\x1b[0m)\n'.format(dim.rows, dim.columns))

            self.stdout.write('\n')

    def do_display(self, s):
        sel = self._select(s, Cdn.SelectorType.VARIABLE)

        if sel is None:
            return

        self._display_vars([x.get_object() for x in sel])

    ansire = re.compile(r'\x1b[^m]*m')

    def _strlen(self, s):
        # Strip ansi codes
        return len(self.ansire.sub(u'', s))

    def _makeutf8(self, s):
        if hasattr(s, 'decode'):
            return s.decode('utf-8')

        return u'{0}'.format(s)

    def columnize(self, items, columns=-1):
        if columns <= 0:
            try:
                (_, columns) = [int(x) for x in os.popen('stty size', 'r').read().split()]
            except Exception as e:
                columns = 80

        items = [self._makeutf8(x) for x in items]
        lens = [self._strlen(x) for x in items]

        maxlen = max(lens)

        ncols = int(math.floor(columns / (maxlen + 4)))
        nrows = int(math.ceil(float(len(items)) / ncols))

        colsize = int(math.floor(columns / ncols))

        for r in range(0, nrows):
            for c in range(0, ncols):
                i = r + c * nrows

                if i >= len(items):
                    continue

                item = items[i]

                self.stdout.write(item)
                self.stdout.write(' ' * (colsize - lens[i]))

            self.stdout.write('\n')

    def _error(self, s):
        self.stdout.write('\x1b[31m{0}\x1b[0m\n'.format(s))

    def do_ls(self, s):
        objects = filter(lambda x: isinstance(x, Cdn.Object), [x.get_object() for x in self._selections])
        isone = len(objects) == 1

        for o in objects:
            if not isone:
                sys.stdout.write('[{0}]\n'.format(o.get_full_id_for_display()))

            objs = ['\x1b[34m{0}\x1b[0m'.format(c.get_id()) for c in o.get_children()]
            vars = ['\x1b[32m{0}\x1b[0m'.format(v.get_name()) for v in o.get_variables()]

            self.columnize(objs + vars)

    def do_load(self, s):
        self.load_network(s)

    def load_network(self, s):
        n = Cdn.Network()

        try:
            n.load_from_path(s)
        except Exception as e:
            self._error('Error loading network: {0}'.format(e.message))
            return False

        err = Cdn.CompileError()

        if not n.compile(None, err):
            self._error('Error compiling network: {0}'.format(err.get_formatted_string()))
            return False

        self.filename = s
        self.network = n

        self._selections = None
        self._set_selections()
        self._watch = set()

        return True

    def complete_load(self, text, line, begidx, endidx):
        return self._complete_filename(text)

    def do_r(self, s):
        self.do_reload(s)

    def _reload_id(self, obj):
        if isinstance(obj, Cdn.Network):
            return None
        else:
            return obj.get_full_id()

    def do_reload(self, s):
        if self.filename is None:
            self._error('No network has been loaded yet')
            return

        objs = filter(lambda x: isinstance(x, Cdn.Object), [x.get_object() for x in self._selections])

        names = [self._reload_id(x) for x in objs]
        watches = [v.get_full_name() for v in self._watch]

        if self.load_network(self.filename):
            newsel = []

            for name in names:
                if name is None:
                    obj = self.network
                else:
                    try:
                        obj = self.network.find_object(name)
                    except:
                        obj = None

                if not obj is None:
                    newsel.append(Cdn.Selection.new(obj, None))

            self._set_selections(newsel)

            for watch in watches:
                try:
                    v = self.network.find_variable(watch)
                except:
                    v = None

                if not v is None:
                    self._watch.add(v)

    def _select(self, s, seltype=Cdn.SelectorType.ANY):
        s = s.strip()
        nochild = s.startswith('..')

        if s.endswith('..'):
            s = s[:-2] + ' | parent'

        s = s.replace('..', ' | parent .')

        try:
            selector = Cdn.Selector.parse(self.network, s)
            selector.set_implicit_children(not nochild)
        except Exception as e:
            self._error('Failed to parse selector: {0}'.format(e.message))
            return None

        return selector.select_set(self._selections, seltype)

    def do_cd(self, s):
        s = s.strip()

        if len(s) == 0:
            self._set_selections()
            return
        elif s == '-':
            self._set_selections(list(self._prev_selections))
            return

        sel = self._select(s, Cdn.SelectorType.OBJECT)

        if sel is None:
            return

        if len(sel) == 0:
            self._error('Did not find any nodes matching this selector')
        else:
            self._set_selections(sel)

    def do_step(self, s):
        s = s.strip()

        if len(s) == 0:
            dt = self.network.get_integrator().get_default_timestep()
        else:
            try:
                dt = float(s)
            except:
                self._error('Invalid timestep')
                return

        if dt <= 0:
            self._error('Please provide a timestep > 0')
            return

        self.network.step(dt)
        self._display_vars(self._watch, True)

        self._update_prompt()

    def do_run(self, s):
        s = s.strip()

        if len(s) == 0:
            self._error('Please provide a time range to run')
            return

        start = 0
        step = self.network.get_integrator().get_default_timestep()

        parts = s.split(':')

        if len(parts) == 3:
            (start, step, end) = parts
        elif len(parts) == 2:
            (start, end) = parts
        else:
            (end,) = parts

        try:
            start = float(start)
        except:
            self._error('Invalid start time')
            return

        try:
            step = float(step)
        except:
            self._error('Invalid step')
            return

        try:
            end = float(end)
        except:
            self._error('Invalid end time')
            return

        if start > end:
            self._error('Start time must be before end time')
            return

        if step <= 0:
            self._error('Timestep must be larger than 0')
            return

        self.network.run(start, step, end)
        self._display_vars(self._watch, True)

        self._update_prompt()

    def do_w(self, s):
        self.do_watch(s)

    def do_watch(self, s):
        sel = self._select(s, Cdn.SelectorType.VARIABLE)

        if sel is None:
            return

        if len(sel) == 0:
            self._error('Did not find any variables matching this selector')
            return

        for o in sel:
            self._watch.add(o.get_object())

    def do_uw(self, s):
        self.do_unwatch(s)

    def do_unwatch(self, s):
        sel = self._select(s, Cdn.SelectorType.VARIABLE)

        if sel is None:
            return

        if len(sel) == 0:
            self._error('Did not find any variables matching this selector')
            return

        for o in sel:
            o = o.get_object()

            try:
                self._watch.remove(o)
            except KeyError:
                self._error('The variable `{0}\' was not being watched'.format(o.get_full_name_for_display()))

    def do_reset(self, s):
        self.network.reset()
        self._update_prompt()

    def do_EOF(self, s):
        self.stdout.write('\n')
        sys.exit(1)

# vi:ts=4:et
