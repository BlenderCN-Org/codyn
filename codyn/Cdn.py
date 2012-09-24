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

def flat_to_matrix(ret, dims):
    if len(ret) == 1:
        return ret

    rret = []

    for i in range(0, dims[0]):
        start = i * dims[1]
        end = start + dims[1]

        rret.extend(ret[start:end])

    return rret

class Variable(Cdn.Variable):
    def get_values(self):
        ret = self.get_values_flat()
        dims = self.get_dimension()

        return flat_to_matrix(ret, dims)

    def set_values(self, vals, numr, numc):
        self.set_values_flat(vals, numr, numc)

Variable = override(Variable)
__all__.append('Variable')

class Expression(Cdn.Expression):
    def get_values(self):
        ret = self.get_values_flat()
        dims = self.get_dimension()

        return flat_to_matrix(ret, dims)

    def set_values(self, vals, numr, numc):
        self.set_values_flat(vals, numr, numc)

Expression = override(Expression)
__all__.append('Expression')

# vi:ex:ts=4:et
