import bgl, blf, inspect, os, re, json

class Point:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return isinstance(other, self.__class__) \
            and other.x == self.x \
            and other.y == self.y

    def __ne__(self, other):
        return not self.__eq__(other)

    def __iadd__(self, other):
        if isinstance(other, Point):
            self.x += other.x
            self.y += other.y
        else:
            self.x += other
            self.y += other

        return self

    def __add__(self, other):
        ret = Point(self.x, self.y)
        ret += other

        return ret

    def __isub__(self, other):
        if isinstance(other, Point):
            self.x -= other.x
            self.y -= other.y
        else:
            self.x -= other
            self.y -= other

        return self

    def __sub__(self, other):
        ret = Point(self.x, self.y)
        ret -= other

        return ret

    def __imul__(self, other):
        if isinstance(other, Point):
            self.x *= other.x
            self.y *= other.y
        else:
            self.x *= other
            self.y *= other

        return self

    def __mul__(self, other):
        ret = Point(self.x, self.y)
        ret *= other

        return ret

    def __itruediv__(self, other):
        if isinstance(other, Point):
            self.x /= other.x
            self.y /= other.y
        else:
            self.x /= other
            self.y /= other

        return self

    def __truediv__(self, other):
        ret = Point(self.x, self.y)
        ret /= other

        return ret

    def __neg__(self):
        return Point(-self.x, -self.y)

    def __repr__(self):
        return '<{0} at 0x{1:x}, [x={2}, y={3}]>'.format(self.__class__.__name__, id(self), self.x, self.y)

class Rect:
    def __init__(self, x=0, y=0, w=0, h=0):
        self.x = x
        self.y = y
        self.w = w
        self.h = h

    @staticmethod
    def from_array(v):
        return Rect(v[0], v[1], v[2], v[3])

    @staticmethod
    def uniform(v):
        return Rect(v, v, v, v)

    def __eq__(self, other):
        return isinstance(other, Rect) \
            and other.x == self.x \
            and other.y == self.y \
            and other.w == self.w \
            and other.h == self.h

    def __ne__(self, other):
        return not self.__eq__(other)

    def reduce(self, other):
        return Rect(self.x + other.x,
                    self.y + other.y,
                    self.w - other.sx,
                    self.h - other.sy)

    def hittest(self, p):
        return p.x >= self.x and \
               p.x < self.x + self.w and \
               p.y >= self.y and \
               p.y < self.y + self.h

    def __repr__(self):
        return '<{0} at 0x{1:x}, [x={2}, y={3}, w={4}, h={5}]>'.format(self.__class__.__name__, id(self), self.x, self.y, self.w, self.h)

    @property
    def xy(self):
        return Point(self.x, self.y)

    @property
    def xw(self):
        return Point(self.x, self.w)

    @property
    def yh(self):
        return Point(self.y, self.h)

    @property
    def wh(self):
        return Point(self.w, self.h)

    @property
    def sx(self):
        return self.x + self.w

    @property
    def sy(self):
        return self.y + self.h

    @property
    def sxsy(self):
        return Point(self.x + self.w, self.y + self.h)

class Color:
    def __init__(self, r=0, g=0, b=0, a=0):
        self.r = r
        self.g = g
        self.b = b
        self.a = a

    def __eq__(self, other):
        return isinstance(other, Color) \
            and other.r == self.r \
            and other.g == self.g \
            and other.b == self.b \
            and other.a == self.a

    def __ne__(self, other):
        return not self.__eq__(other)

    def apply(self):
        bgl.glColor4f(self.r, self.g, self.b, self.a)

class Alignment(Point):
    pass

class Fill(Point):
    def __init__(self, x=False, y=False):
        Point.__init__(self, x, y)

class Texture:
    atlas = re.compile('(.*[.]atlas):([a-z_][a-z0-9_]*)')

    def __init__(self, filename):
        from bge import texture

        self.region = None

        if not filename.startswith('/'):
            f = inspect.getframeinfo(inspect.currentframe()).filename
            d = os.path.dirname(f)

            filename = os.path.join(d, 'data', filename)

        m = Texture.atlas.match(filename)

        self.filename = filename

        if not m is None:
            self.setup_atlas(m.group(1), m.group(2))
        else:
            self.source = texture.ImageFFmpeg(self.filename)
            self.buffer = texture.imageToArray(self.source, 'RGBA')

            if self.buffer is None:
                print('Error loading {0}: {1}'.format(filename, texture.getLastError()))

            self.glid = bgl.Buffer(bgl.GL_INT, 1)
            bgl.glGenTextures(1, self.glid)

            self.bind()

            bgl.glTexEnvi(bgl.GL_TEXTURE_ENV, bgl.GL_TEXTURE_ENV_MODE, bgl.GL_REPLACE)
            bgl.glTexParameteri(bgl.GL_TEXTURE_2D, bgl.GL_TEXTURE_MIN_FILTER, bgl.GL_NEAREST)
            bgl.glTexParameteri(bgl.GL_TEXTURE_2D, bgl.GL_TEXTURE_MAG_FILTER, bgl.GL_NEAREST)

            bgl.glTexImage2D(bgl.GL_TEXTURE_2D,
                             0,
                             bgl.GL_RGBA,
                             self.width,
                             self.height,
                             0,
                             bgl.GL_RGBA,
                             bgl.GL_UNSIGNED_BYTE,
                             self.buffer)

            self.unbind()

    def setup_atlas(self, filename, region):
        with open(filename, 'r') as doc:
            d = json.load(doc)

            atlas = Textures.load(d['filename'])

            self.glid = atlas.glid
            self.source = atlas.source
            self.buffer = atlas.buffer

            if region in d['regions']:
                self.region = Rect.from_array([float(x) for x in d['regions'][region]])

                # Normalize as texture coordinates
                self.region.x /= self.source.size[0]
                self.region.y = (self.source.size[1] - self.region.y - self.region.h) / self.source.size[1]
                self.region.w /= self.source.size[0]
                self.region.h /= self.source.size[1]
            else:
                self.region = None

    @property
    def wh(self):
        return Point(self.width, self.height)

    @property
    def width(self):
        if self.region is None:
            return self.source.size[0]
        else:
            return self.region.w * self.source.size[0]

    @property
    def height(self):
        if self.region is None:
            return self.source.size[1]
        else:
            return self.region.h * self.source.size[1]

    def bind(self):
        bgl.glBindTexture(bgl.GL_TEXTURE_2D, self.glid[0])

        if not self.region is None:
            bgl.glMatrixMode(bgl.GL_TEXTURE)
            bgl.glLoadIdentity()
            bgl.glTranslatef(self.region.x, self.region.y, 0)
            bgl.glScalef(self.region.w, self.region.h, 1)
            bgl.glMatrixMode(bgl.GL_MODELVIEW)

    def unbind(self):
        bgl.glBindTexture(bgl.GL_TEXTURE_2D, 0)

        if not self.region is None:
            bgl.glMatrixMode(bgl.GL_TEXTURE)
            bgl.glLoadIdentity()
            bgl.glMatrixMode(bgl.GL_MODELVIEW)

class Textures:
    textures = {}

    @staticmethod
    def load(filename):
        if len(Textures.textures) == 0:
            bgl.glEnable(bgl.GL_TEXTURE_2D)

        if filename in Textures.textures:
            return Textures.textures[filename]

        t = Texture(filename)
        Textures.textures[filename] = t

        return t

class BoxTexture:
    def __init__(self, filename, xregions, yregions):
        self.texture = Textures.load(filename)

        self.xregions = xregions
        self.yregions = yregions

        self.xfixed = 0
        self.nxstretch = 0

        self.yfixed = 0
        self.nystretch = 0

        self.color = Color(1, 1, 1, 1)

        for x in self.xregions:
            if x >= 0:
                self.xfixed += x
            else:
                self.nxstretch += 1

        for y in self.yregions:
            if y >= 0:
                self.yfixed += y
            else:
                self.nystretch += 1

    def draw(self, w, h):
        if self.nxstretch > 0:
            sx = (w - self.xfixed) / self.nxstretch
            tx = (self.texture.width - self.xfixed) / self.nxstretch

        if self.nystretch > 0:
            sy = (h - self.yfixed) / self.nystretch
            ty = (self.texture.height - self.yfixed) / self.nystretch

        y0 = 0
        ty0 = 0

        l = lambda x, sx: {True: sx, False: x}[x < 0]

        self.texture.bind()

        bgl.glBegin(bgl.GL_QUADS)
        self.color.apply()

        for y in self.yregions:
            x0 = 0
            tx0 = 0

            dy = l(y, sy)
            dty = l(y, ty) / self.texture.height

            for x in self.xregions:
                dx = l(x, sx)
                dtx = l(x, tx) / self.texture.width

                bgl.glTexCoord2f(tx0, ty0)
                bgl.glVertex2f(x0, y0)

                bgl.glTexCoord2f(tx0 + dtx, ty0)
                bgl.glVertex2f(x0 + dx, y0)

                bgl.glTexCoord2f(tx0 + dtx, ty0 + dty)
                bgl.glVertex2f(x0 + dx, y0 + dy)

                bgl.glTexCoord2f(tx0, ty0 + dty)
                bgl.glVertex2f(x0, y0 + dy)

                x0 += dx
                tx0 += dtx

            y0 += dy
            ty0 += dty

        bgl.glEnd()

        self.texture.unbind()

class Widget(object):
    def __init__(self, **kwargs):
        self.allocation = Rect()
        self.children = []
        self.parent = None
        self._relayout = False
        self.padding = Rect()
        self.margin = Rect()
        self.alignment = Alignment()
        self.fill = Fill()
        self.background = None
        self.color = Color(1, 1, 1, 1)
        self._debug = False
        self.mouse_focus = False

        self.setup()

        for k in kwargs:
            setattr(self, k, kwargs[k])

    def on_mouse_focus_in(self, p):
        self.mouse_focus = True
        return True

    def on_mouse_focus_out(self, p):
        self.mouse_focus = False
        return True

    def on_button_press(self, p):
        return False

    def on_button_release(self, p):
        return False

    def hittest(self, p):
        if not self.allocation.hittest(p):
            return None, Point()

        pc = p - self.allocation.xy

        # Check children
        for child in self.children:
            w, retp = child.hittest(pc)

            if not w is None:
                return w, retp

        return self, p

    def setup(self):
        pass

    def queue_resize(self):
        self._relayout = True

        if not self.parent is None:
            self.parent.queue_resize()

    def child_placement(self, child, alloc, childreq=None):
        if childreq is None:
            childreq = child.size_request()

        # Calculate effective width/height of allocation area
        es = Point()

        if child.fill.x:
            es.x = alloc.w - child.margin.sx
        else:
            es.x = childreq.x

        if child.fill.y:
            es.y = alloc.h - child.margin.sy
        else:
            es.y = childreq.y

        pc = child.alignment * es
        pp = child.alignment * (alloc.wh - child.margin.sxsy)

        v = pp - pc + child.margin.xy

        return Rect(alloc.x + v.x,
                    alloc.sy - v.y - es.y,
                    es.x,
                    es.y)

    def size_request(self):
        ret = Point(self.padding.sx, self.padding.sy)
        maxd = Point()

        for child in self.children:
            req = child.size_request()

            maxd.x = max(maxd.x, req.x + child.margin.sx)
            maxd.y = max(maxd.y, req.y + child.margin.sy)

        return ret + maxd

    def add(self, child):
        if child.parent == self:
            return

        if not child.parent is None:
            child.parent.remove(child)

        self.children.append(child)
        child.parent = self

        self._relayout = True

    def remove(self, child):
        if child.parent == self:
            self.children.remove(child)
            child.parent = None

    def allocate(self, a):
        if a != self.allocation:
            self.allocation = a

            for child in self.children:
                child.queue_layout()

    def queue_layout(self):
        self._relayout = True

    def layout_intern(self):
        area = Rect(self.padding.x,
                    self.padding.y,
                    self.allocation.w - self.padding.sx,
                    self.allocation.h - self.padding.sy)

        for child in self.children:
            a = self.child_placement(child, area)

            child.allocate(a)
            child.layout()

    def layout(self):
        if self._relayout:
            self.layout_intern()
            self._relayout = False

    def draw_debug(self):
        w = self.allocation.w
        h = self.allocation.h

        bgl.glColor3f(1, 0, 0)

        bgl.glBegin(bgl.GL_LINE_STRIP)
        bgl.glVertex2f(0, 0)
        bgl.glVertex2f(w, 0)
        bgl.glVertex2f(w, h)
        bgl.glVertex2f(0, h)
        bgl.glVertex2f(0, 0)
        bgl.glEnd()

        bgl.glColor3f(0.5, 0, 0.5)

        bgl.glBegin(bgl.GL_LINE_STRIP)
        bgl.glVertex2f(self.padding.x, self.padding.h)
        bgl.glVertex2f(w - self.padding.w, self.padding.h)
        bgl.glVertex2f(w - self.padding.w, h - self.padding.y)
        bgl.glVertex2f(self.padding.x, h - self.padding.y)
        bgl.glVertex2f(self.padding.x, self.padding.h)
        bgl.glEnd()

    def draw(self):
        if not self.background is None:
            self.background.draw(self.allocation.w, self.allocation.h)

        if self.debug:
            self.draw_debug()

        for child in self.children:
            bgl.glPushMatrix()

            bgl.glTranslatef(child.allocation.x,
                             child.allocation.y,
                             0)

            child.draw()

            bgl.glPopMatrix()

    @property
    def debug(self):
        return self._debug

    @debug.setter
    def debug(self, v):
        self._debug = v

        for child in self.children:
            child.debug = v

class Image(Widget):
    def setup(self):
        self._filename = None
        self.texture = None

    @property
    def filename(self):
        return self._filename

    @filename.setter
    def filename(self, v):
        if self._filename == v:
            return

        self._filename = v
        self.texture = Textures.load(v)

        self.queue_resize()

    def size_request(self):
        return self.padding.sxsy + self.texture.wh

    def draw(self):
        Widget.draw(self)

        if self.texture is None:
            return

        self.texture.bind()

        bgl.glBegin(bgl.GL_QUADS)
        self.color.apply()

        bgl.glTexCoord2f(0, 0)
        bgl.glVertex2f(self.padding.x, self.padding.h)

        bgl.glTexCoord2f(1, 0)
        bgl.glVertex2f(self.allocation.w - self.padding.w, self.padding.h)

        bgl.glTexCoord2f(1, 1)
        bgl.glVertex2f(self.allocation.w - self.padding.w, self.allocation.h - self.padding.y)

        bgl.glTexCoord2f(0, 1)
        bgl.glVertex2f(self.padding.x, self.allocation.h - self.padding.y)

        bgl.glEnd()
        self.texture.unbind()

class Label(Widget):
    def setup(self):
        self._text = ''
        self._size = 10
        self.color = Color(0, 0, 0, 1)
        self._font = 0
        self._fontname = None
        self._dpi = 96

    @property
    def text(self):
        return self._text

    @text.setter
    def text(self, v):
        if self._text == v:
            return

        self._text = v
        self.queue_resize()

    @property
    def size(self):
        return self._size

    @size.setter
    def size(self, v):
        if self._size == v:
            return

        self._size = v
        self.queue_resize()

    @property
    def font(self):
        return self._fontname

    @font.setter
    def font(self, v):
        if self._fontname == v:
            return

        self._fontname = v
        self._font = blf.load(self._fontname)

        if self._font < 0:
            self._font = 0

        self.queue_resize()

    def draw(self):
        Widget.draw(self)

        self.color.apply()

        blf.position(self._font, self.padding.x, self.padding.y, 0)
        blf.size(self._font, self.size, self._dpi)
        blf.draw(self._font, self.text)

    def size_request(self):
        blf.size(self._font, self.size, self._dpi)
        dim = blf.dimensions(self._font, self.text)

        return Point(dim[0] + self.padding.sx, dim[1] + self.padding.sy)

class Box(Widget):
    VERTICAL = 0
    HORIZONTAL = 1

    def __init__(self, **kwargs):
        self._orientation = Box.VERTICAL
        self._homogeneous = False
        self._spacing = 0

        Widget.__init__(self, **kwargs)

    @property
    def orientation(self):
        return self._orientation

    @orientation.setter
    def orientation(self, value):
        if self._orientation == value:
            return

        self._orientation = value
        self.queue_layout()

    @property
    def homogeneous(self):
        return self._homogeneous

    @homogeneous.setter
    def homogeneous(self, value):
        if self._homogeneous == value:
            return

        self._homogeneous = value
        self.queue_layout()

    @property
    def spacing(self):
        return self._spacing

    @orientation.setter
    def spacing(self, value):
        if self._spacing == value:
            return

        self._spacing = value
        self.queue_layout()

    def layout_intern(self):
        if len(self.children) == 0:
            return

        dim = self.padding.sxsy

        sx = self.padding.x
        sy = self.allocation.h - self.padding.h

        reqs = [child.size_request() for child in self.children]

        if self._homogeneous:
            childs = (self.allocation.wh - dim) / len(self.children)

            childs = [childs] * len(self.children)
        else:
            sums = Point()
            fixed = Point()

            for i in range(0, len(self.children)):
                req = reqs[i]
                child = self.children[i]

                if child.fill.x:
                    sums.x += child.margin.sx + req.x
                else:
                    fixed.x += child.margin.sx + req.x

                if child.fill.y:
                    sums.y += child.margin.sy + req.y
                else:
                    fixed.y += child.margin.sy + req.y

            childs = [None] * len(self.children)
            flex = (self.allocation.wh - dim) - fixed

            if self._orientation == Box.VERTICAL:
                flex.y -= max(0, len(self.children) - 1) * self._spacing
            else:
                flex.x -= max(0, len(self.children) - 1) * self._spacing

            for i in range(0, len(self.children)):
                req = reqs[i]
                child = self.children[i]

                childs[i] = Point()

                if child.fill.x:
                    childs[i].x = flex.x * (child.margin.sx + req.x) / sums.x
                else:
                    childs[i].x = child.margin.sx + req.x

                if child.fill.y:
                    childs[i].y = flex.y * (child.margin.sy + req.y) / sums.y
                else:
                    childs[i].y = child.margin.sy + req.y

        for i in range(0, len(self.children)):
            req = reqs[i]
            child = self.children[i]

            if self._orientation == Box.VERTICAL:
                sy -= childs[i].y

                area = Rect(sx, sy, self.allocation.w - self.padding.sx, childs[i].y)

                child.allocate(self.child_placement(child, area, reqs[i]))

                sy -= self._spacing
            else:
                area = Rect(sx, self.padding.h, childs[i].x, self.allocation.h - self.padding.sy)
                child.allocate(self.child_placement(child, area, reqs[i]))

                sx += childs[i].x + self._spacing

            child.layout()

    def size_request(self):
        mw = 0
        mh = 0

        for child in self.children:
            req = child.size_request()

            cw = req.x + child.margin.sx
            ch = req.y + child.margin.sy

            if self._orientation == Box.VERTICAL:
                mw = max(mw, cw)
                mh += ch
            else:
                mw += cw
                mh = max(mh, ch)

        return Point(self.padding.sx + mw, self.padding.sy + mh)

class Button(Widget):
    def setup(self):
        self.background = BoxTexture('gui.atlas:roundedrect', [3, -1, 3], [3, -1, 3])
        self.padding = Rect.uniform(6)
        self._icon = None
        self._widget = None
        self._text = None

    @property
    def icon(self):
        return self._icon

    @icon.setter
    def icon(self, v):
        if self._icon == v:
            return

        self._icon = v

        if not isinstance(v, Widget):
            v = Image(filename=v)
            v.alignment = Alignment(0.5, 0.5)

        if not self._widget is None:
            self.remove(self._widget)

        self._widget = v
        self.add(v)
        self.queue_resize()

    @property
    def text(self):
        return self._text

    @text.setter
    def text(self, v):
        if self._text == v:
            return

        self._text = v

        if not isinstance(v, Widget):
            v = Label(text=v)
            v.alignment = Alignment(0.5, 0.5)

        if not self._widget is None:
            self.remove(self._widget)

        self._widget = v
        self.add(v)
        self.queue_resize()

    @property
    def widget(self):
        return self._widget

    @widget.setter
    def widget(self, v):
        if self._widget == v:
            return

        if not self._widget is None:
            self.remove(self._widget)

        self._widget = v
        self.add(v)
        self.queue_resize()

class Screen(Widget):
    def __init__(self):
        Widget.__init__(self)

        from bge import render
        from bge import logic
        from bge import events

        self._render = render
        self._logic = logic
        self._events = events

        self._mouse_focus = None
        self._mouse_down = {}

    def emit_event(self, w, cb, e):
        while not w is None:
            if cb(w, e):
                return w

            w = w.parent

        return None

    def process_mouse_move(self, xy):
        widget, p = self.hittest(xy)

        if widget == self._mouse_focus:
            return

        if not self._mouse_focus is None:
            self._mouse_focus.on_mouse_focus_out(p)

        self._mouse_focus = widget

        if not widget is None:
            self._mouse_focus = self.emit_event(widget, lambda x, e: x.on_mouse_focus_in(e), p)
        else:
            self._mouse_focus = None

    def global_to_widget_coords(self, w, xy):
        xyret = Point(xy.x, xy.y)

        while not w is None:
            xyret -= w.allocation.xy
            w = w.parent

        return xyret

    def process_mouse_updown(self, xy, button):
        active = self._logic.mouse.events[button] == self._logic.KX_INPUT_ACTIVE

        if active and not button in self._mouse_down:
            w, p = self.hittest(xy)

            if not w is None:
                self._mouse_down[button] = self.emit_event(w, lambda x, e: x.on_button_press(e), p)
            else:
                self._mouse_down[button] = None
        elif not active and button in self._mouse_down:
            if not self._mouse_down[button] is None:
                self._mouse_down[button].on_button_release(self.global_to_widget_coords(self._mouse_down[button], xy))

            del self._mouse_down[button]

    def process_events(self):
        m = self._logic.mouse
        pos = m.position

        p = Point(pos[0] * self._render.getWindowWidth(),
                  (1 - pos[1]) * self._render.getWindowHeight())

        if self._events.MOUSEX in m.active_events or self._events.MOUSEY in m.active_events:
            self.process_mouse_move(p)

        ev = [self._events.LEFTMOUSE, self._events.MIDDLEMOUSE, self._events.RIGHTMOUSE]

        for e in ev:
            if e in m.active_events:
                self.process_mouse_updown(p, e)
            elif e in self._mouse_down:
                self.process_mouse_updown(p, e)

    def update(self):
        self.process_events()
        self.draw()

    def draw(self):
        self.allocate(Rect(self.margin.x,
                           self.margin.y,
                           self._render.getWindowWidth() - self.margin.sx,
                           self._render.getWindowHeight() - self.margin.sy))
        self.layout()

        bgl.glMatrixMode(bgl.GL_PROJECTION)
        bgl.glLoadIdentity()
        bgl.gluOrtho2D(0, self._render.getWindowWidth(), 0, self._render.getWindowHeight())
        bgl.glMatrixMode(bgl.GL_MODELVIEW)
        bgl.glLoadIdentity()

        bgl.glPushMatrix()
        bgl.glTranslatef(self.margin.x, self.margin.h, 0)

        bgl.glEnable(bgl.GL_TEXTURE_2D)
        bgl.glEnable(bgl.GL_BLEND)
        bgl.glDisable(bgl.GL_DEPTH)
        bgl.glDisable(bgl.GL_LIGHTING)

        Widget.draw(self)

        bgl.glPopMatrix()

# vi:ts=4:et
