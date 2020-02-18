from protcur.config import __units_folder__ as units_path
from pysercomb.parsers.units import get_unit_dicts, make_unit_parser
from pysercomb.types import intc, strc


def chain(*tups):
    for t in tups:
        yield from reversed(t)  # reversed because the piority ordering is inverted


class ImplFactoryHelper:
    # FIXME NOTE
    # UnitsParser is a case where
    @classmethod
    def bindImpl(cls, class_name, *classes):
        """ returns a new interpreter bound to a particular implementation of
            Unit

            specify the underlying python classes that the parser targets
            this is sort of like being able to change the #lang you are using """
        if class_name is None:
            for cls_ in classes:
                setattr(cls, '_' + cls_.__name__, cls_)

        else:
            class_dict = {'_' + cls.__name__:cls for cls in classes}
            return type(class_name, (cls,), class_dict)


class UnitsHelper:

    __setup = False

    @staticmethod
    def setup():
        """ call setup on UnitsHelper once, not on subclasses """

        if hasattr(UnitsHelper, '__setup') and UnitsHelper.__setup:
            return  # already setup

        UnitsHelper.__setup = True

        dicts = get_unit_dicts(units_path)

        (parameter_expression, quantity, unit, *_,
         debug_dict) = make_unit_parser(dicts=dicts)

        UnitsHelper._parameter_expression = staticmethod(parameter_expression)

        gs = globals()
        for dict_ in dicts:
            gs.update(dict_)

        UnitsHelper.si_exponents = {prefix if prefix is None else strc(prefix):intc(exp)
                            for prefix, exp in prefixes_si_exponents}
        UnitsHelper.si_exponents_inv = {e:p for p, e in UnitsHelper.si_exponents.items()}

        UnitsHelper.unit_dict = {strc(unit):strc(abbrev) for abbrev, unit in
                          chain(units_si,
                                units_extra,
                                units_extra_prefix,
                                units_dimensionless,
                                units_dimensionless_prefix,
                                units_imp,)}

        UnitsHelper.unit_dict_inv = {a:u for u, a in UnitsHelper.unit_dict.items()}

        # don't actually need this because its in the ast
        #prefix_units = set(unit for abbrev, unit in
                        #chain(units_extra_prefix,
                                #units_dimensionless_prefix))

        UnitsHelper.prefix_dict = {strc(prefix):strc(abbrev) for abbrev, prefix in prefixes_si}
        UnitsHelper.prefix_dict_inv = {p:a for a, p in UnitsHelper.prefix_dict.items()}

        # surely there is a more elegant way ...
        # TODO
        UnitsHelper.conversion = {'__truediv__':[],
                                  '__mul__':[]}


class Expr(UnitsHelper, ImplFactoryHelper):
    op = None

    def __add__(self, other):
        return self._Add(self, other)

    def __iadd__(self, other):
        return self.__add__(other)

    #def __mul__(self, other):
        #return self._Mul(self, other)

    def __imul__(self, other):
        return self.__mul__(other)

    def __truediv__(self, other):
        return self._Div(self, other)

    def __pow__(self, other):
        return self._Exp(self, other)

    def __lt__(self, other):
        if other is LessThan:
            return LessThan(None, other)
        else:
            raise NotImplementedError

    def __gt__(self, other):
        if other is GreaterThan:
            return GreaterThan(None, other)
        else:
            raise NotImplementedError

    #@express
    #def asRdf(self):
        #yield from self.asRdf(rdflib.BNode())

    @property
    def ttl(self):
        graph = rdflib.Graph()
        OntCuries.populate(graph)
        [graph.add(t) for t in self.asRdf()]
        return graph.serialize(format='nifttl')
