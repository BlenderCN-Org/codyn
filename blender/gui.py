import bgl, blf, inspect, os

class Rect:
    def __init__(self, x=0, y=0, w=0, h=0):
        self.x = x
        self.y = y
        self.w = w
        self.h = h

    def __eq__(self, other):
        return isinstance(other, Rect) \
            and other.x == self.x \
            and other.y == self.y \
            and other.w == self.w \
            and other.h == self.h

    def __ne__(self, other):
        return not self.__eq__(other)

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

class Alignment:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return isinstance(other, Alignment) \
            and other.x == self.x \
            and other.y == self.y

    def __ne__(self, other):
        return not self.__ne__(other)

class Fill:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return isinstance(other, Fill) \
            and other.x == self.x \
            and other.y == self.y

    def __ne__(self, other):
        return not self.__ne__(other)

class Texture:
    def __init__(self, filename):
        from bge import texture

        if not filename.startswith('/'):
            f = inspect.getframeinfo(inspect.currentframe()).filename
            d = os.path.dirname(f)

            filename = os.path.join(d, 'data', filename)

        self.filename = filename
        self.source = texture.ImageFFmpeg(self.filename)
        self.buffer = texture.imageToArray(self.source, 'RGBA')

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

    @property
    def width(self):
        return self.source.size[0]

    @property
    def height(self):
        return self.source.size[1]

    def bind(self):
        bgl.glBindTexture(bgl.GL_TEXTURE_2D, self.glid[0])

    def unbind(self):
        bgl.glBindTexture(bgl.GL_TEXTURE_2D, 0)

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
        bgl.glColor4f(self.color.r, self.color.g, self.color.b, self.color.a)

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
    def __init__(self):
        self.allocation = Rect()
        self.children = []
        self.parent = None
        self._relayout = False
        self.padding = Rect()
        self.margin = Rect()
        self.align = Alignment()
        self.fill = Fill()
        self.background = None

        self.debug = False

    def size_request(self):
        return (self.padding.x + self.padding.w, self.padding.y + self.padding.h)

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
        pass

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

class Label(Widget):
    def __init__(self, text='', size=10, font=0):
        Widget.__init__(self)

        self.text = text
        self.size = size
        self.color = Color(0, 0, 0, 1)

        self._font = font
        self._dpi = 96

    def draw(self):
        Widget.draw(self)

        bgl.glColor4f(self.color.r, self.color.g, self.color.b, self.color.a)

        blf.position(self._font, self.padding.x, self.padding.y, 0)
        blf.size(self._font, self.size, self._dpi)
        blf.draw(self._font, self.text)

    def size_request(self):
        blf.size(self._font, self.size, self._dpi)
        dim = blf.dimensions(self._font, self.text)

        return (dim[0] + self.padding.x + self.padding.w, dim[1] + self.padding.y + self.padding.h)

class Box(Widget):
    VERTICAL = 0
    HORIZONTAL = 1

    def __init__(self, orientation=VERTICAL):
        Widget.__init__(self)

        self._orientation = orientation
        self._homogeneous = False

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

    def layout_intern(self):
        if len(self.children) == 0:
            return

        dim = [self.padding.x + self.padding.w, self.padding.y + self.padding.h]

        sx = self.padding.x
        sy = self.padding.y

        reqs = [child.size_request() for child in self.children]

        if self._homogeneous:
            childw = (self.allocation.w - dim[0]) / len(self.children)
            childh = (self.allocation.h - dim[1]) / len(self.children)

            childw = [childw] * len(self.children)
            childh = [childh] * len(self.children)
        else:
            sums = [0, 0]

            for i in range(0, len(self.children)):
                req = reqs[i]
                child = self.children[i]

                sums[0] += child.margin.x + child.margin.w + req[0]
                sums[1] += child.margin.x + child.margin.w + req[1]

            childw = [None] * len(self.children)
            childh = [None] * len(self.children)

            for i in range(0, len(self.children)):
                req = reqs[i]
                child = self.children[i]

                fracw = (child.margin.x + child.margin.w + req[0]) / sums[0]
                frach = (child.margin.y + child.margin.h + req[1]) / sums[1]

                childw[i] = (self.allocation.w - dim[0]) * fracw
                childh[i] = (self.allocation.h - dim[1]) * frach

        for i in range(0, len(self.children)):
            req = reqs[i]
            child = self.children[i]

            if self._orientation == Box.VERTICAL:
                child.allocate(Rect(sx + child.margin.x,
                                    sy + child.margin.y,
                                    self.allocation.w - self.padding.x - self.padding.w - child.margin.x - child.margin.w,
                                    childh[i] - child.margin.y - child.margin.h))

                sy += childh[i]
            else:
                child.allocate(Rect(sx + child.margin.x,
                                    sy + child.margin.y,
                                    childw[i] - child.margin.x - child.margin.w,
                                    self.allocation.h - self.padding.y - self.padding.h - child.margin.x - child.margin.w))

                sx += childw[i]

            child.layout()

    def size_request(self):
        mw = 0
        mh = 0

        for child in self.children:
            req = child.size_request()

            cw = req[0] + child.margin.x + child.margin.w
            ch = req[1] + child.margin.y + child.margin.h

            if self._orientation == Box.VERTICAL:
                mw = max(mw, cw)
                mh += ch
            else:
                mw += cw
                mh = max(mh, ch)

        return (self.padding.x + self.padding.w + mw, self.padding.y + self.padding.h + mh)

class Overlay(Widget):
    def __init__(self):
        Widget.__init__(self)

    def layout_intern(self):
        w = self.allocation.w - self.padding.x - self.padding.w
        h = self.allocation.h - self.padding.y - self.padding.h

        for child in self.children:
            req = child.size_request()

            cw = req[0]
            ch = req[1]

            if child.fill.x:
                cw = w

            if child.fill.y:
                cw = h

            sxc = child.margin.x + child.margin.w + cw
            syc = child.margin.y + child.margin.h + ch

            pxc = child.align.x * sxc
            pyc = child.align.y * syc

            px = child.align.x * w + self.padding.x
            py = child.align.y * h + self.padding.y

            # Layout such that pxc aligns with px and pyc aligns with py
            x = px - pxc + child.margin.x
            y = py - pyc + child.margin.y

            child.allocate(Rect(x, self.allocation.h - y - ch, cw, ch))
            child.layout()

class Screen(Overlay):
    def __init__(self):
        Overlay.__init__(self)

        from bge import render
        from bge import logic

        self._render = render
        self._logic = logic

    def draw(self):
        self.allocate(Rect(self.margin.x,
                           self.margin.y,
                           self._render.getWindowWidth() - self.margin.x - self.margin.w,
                           self._render.getWindowHeight() - self.margin.y - self.margin.h))
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

        Overlay.draw(self)

        bgl.glPopMatrix()

# vi:ts=4:et
