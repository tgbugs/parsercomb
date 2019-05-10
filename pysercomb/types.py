try:
    import rdflib
except:
    log.warning('rdflib not found rdf conversion will not work')


class TypeCaster:
    """ base type converter """

    _bases = {}

    @classmethod
    def cast(cls, thing):
        if isinstance(thing, Converter):
            return thing
        else:
            return cls._bases[type(thing)](thing)

class Converter:

    def asRdf(self):
        return rdflib.Literal(self)


class floatc(Converter, float):
    def __truediv__(self, other):
        return floatc(super().__truediv__(other))


class intc(Converter, int):
    def __truediv__(self, other):
        return floatc(super().__truediv__(other))


class strc(Converter, str):
    def __add__(self, other):
        return self.__class__(super().__add__(other))


TypeCaster._bases[float] = floatc
TypeCaster._bases[int] = intc
TypeCaster._bases[str] = strc
