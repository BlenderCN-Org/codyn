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
            return self.get_variable(name)

Object = override(Object)
__all__.append('Object')

class Variable(Cdn.Variable):
    def get_values(self):
        ret = self.get_values_flat()
        dims = self.get_dimension()

        if len(ret) == 1:
            return ret

        rret = []

        for i in range(0, dims[0]):
            start = i * dims[1]
            end = start + dims[1]

            rret.append(ret[start:end])

        return rret

Variable = override(Variable)
__all__.append('Variable')


# vi:ex:ts=4:et
