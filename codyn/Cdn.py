from gi.repository import GObject, Gio
from ..overrides import override
from ..importer import modules
from gi import types

Cdn = modules['Cdn']._introspection_module
__all__ = []

class Object(Cdn.Object):
    def __getattribute__(self, name):
        try:
            return Cdn.Object.__getattribute__(self, name)
        except:
            return self[name]

    def __getitem__(self, name):
        return self.get_variable(name)

Object = override(Object)
__all__.append('Object')

class Node(Cdn.Node):
    def __getitem__(self, name):
        v = self.get_variable(name)

        if v is None:
            return self.get_child(name)
        else:
            return v

Node = override(Node)
__all__.append('Node')


class Matrix(Cdn.Matrix):
    @property
    def values(self):
        ret = self.get_flat()
        dims = self.get_dimension()

        return self._flat_to_matrix(ret, dims)

    def _flat_to_matrix(self, ret, dim):
        if len(ret) == 1:
            return ret[0]

        rret = [None] * dim.rows
        i = 0

        for c in range(0, dims.columns):
            for r in range(0, dims.rows):
                if c == 0:
                    rret[r] = [0] * dims.columns

                rret[r][c] = ret[i]
                i += 1

        return rret


    def set_values(self, vals, numr, numc):
        self.set_values_flat(vals, numr, numc)

Matrix = override(Matrix)
__all__.append('Matrix')

# vi:ex:ts=4:et
