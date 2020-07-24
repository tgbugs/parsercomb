from protcur.config import __units_folder__ as units_path
from pysercomb.parsers.units import get_unit_dicts, make_unit_parser
from pysercomb.types import intc, strc
from . import types as intf


def chain(*tups):
    for t in tups:
        yield from reversed(t)  # reversed because the piority ordering is inverted


class ImplFactoryHelper:
    # FIXME NOTE
    # UnitsParser is a case where
    @classmethod
    def bindImpl(cls, class_name, *classes, **kwargs):
        """ returns a new interpreter bound to a particular implementation of
            Unit

            specify the underlying python classes that the parser targets
            this is sort of like being able to change the #lang you are using """
        if class_name is None:
            for cls_ in classes:
                setattr(cls, '_' + cls_.__name__, cls_)
            for name, some_callable in kwargs.items():
                setattr(cls, '_' + name, some_callable)

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


class Expr(intf.ProtcurExpression, UnitsHelper, ImplFactoryHelper):
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


# comparison classes

class _Than(intf.ProtcurExpression):
    op = None
    tag = None

    def json(self):
        # FIXME prefix vs suffix quantities
        return dict(type=self.tag, value=self.right, unit=self.right.units.json())

    @classmethod
    def fromJson(cls, json):
        assert json['type'] == cls.tag
        #return cls(None, json['value'], json['units'])
        return cls(None, json['value'])  # FIXME units not supported yet

    def to_base_units(self):
        return self.__class__(
            *(_ if _ is None else _.to_base_units() for _ in (self.left, self.right)))

    @property
    def dimensionality(self):
        ld, rd = None, None
        if self.left is not None:
            ld = self.left.dimensionality

        if self.right is not None:
            rd = self.right.dimensionality

        if ld and rd and ld != rd:  # FIXME should catch during construction ya?
            msg = f'{self.__class__.__name__} dimensionality mismatch! {ld} != {rd}'
            raise TypeError(msg)

        return rd if ld is None else ld

    @property
    def units(self):
        lu, ru = None, None
        if self.left is not None:
            lu = self.left.units

        if self.right is not None:
            ru = self.right.units

        if lu and ru and lu != ru:  # FIXME should catch during construction ya?
            msg = f'{self.__class__.__name__} units mismatch! {lu} != {ru}'
            raise TypeError(msg)

        return ru

    def __hash__(self):
        return hash((self.__class__, self.left, self.right))

    def __eq__(self, other):
        # FIXME WARNING assuming that right is always what we wind up with
        return type(self) == type(other) and self.right == other.right

    def __le__(self, other):
        return self < other or self == other

    def __ge__(self, other):
        return self > other or self == other

    def __init__(self, left, right):
        self.left = left
        self.right = right


    def __call__(self, other):
        if self.left is None:
            return getattr(other, self._op)(self)

    def __str__(self):
        left = f'{self.left} ' if self.left else ''
        return f'{left}{self.op} {self.right}'


class ltclass(type):
    _op = '__lt__'
    def __lt__(self, other):
        return self(None, other)


class gtclass(type):
    _op = '__gt__'
    def __gt__(self, other):
        return self(None, other)


class LessThan(_Than, metaclass=ltclass):
    op = '<'
    tag = 'less-than'

    def __lt__(self, other):
        if type(self) == type(other):
            return self.right < other.right
        else:
            # NOTE a bit strange, but
            # < 18 hours can be < 20 minutes
            # of course one might say that on
            # average it is most certainly
            # greater than 20 minutes, but
            # we weren't given a lower bound
            return True

    def __gt__(self, other):
        if type(self) == type(other):
            return self.right > other.right
        else:
            # less thans can never be greater than something else
            # because in theory their value can always be less than
            # any obtained value ... now, in reality most of the time
            # that people use < 18 hours, they are implying that there
            # is a lower bound of zero hours
            return False


class GreaterThan(_Than, metaclass=gtclass):
    op = '>'
    tag = 'greater-than'

    def __lt__(self, other):
        if type(self) == type(other):
            return self.right < other.right
        else:
            return False

    def __gt__(self, other):
        if type(self) == type(other):
            return self.right > other.right
        else:
            return True
