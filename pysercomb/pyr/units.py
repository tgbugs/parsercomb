""" Python class representation for the output of the units parser.
    You must call UnitHelper.setup() in the client code before you can use this. """
import copy
import pprint
import operator
import itertools
from enum import Enum
from urllib.parse import quote, unquote
from pathlib import Path
from numbers import Number
from functools import reduce
import pint
from uncertainties.core import AffineScalarFunc
from pysercomb import exceptions as exc
from pysercomb.utils import log, logd, express
from pysercomb.types import TypeCaster, boolc, strc
from pysercomb.parsers import racket
from pysercomb.parsers.units import _plus_or_minus
from .core import ImplFactoryHelper, UnitsHelper, Expr
from .core import GreaterThan, LessThan
from .core import GreaterThanOrEqual, LessThanOrEqual
from . import types as intf

try:
    import rdflib
    from pyontutils import combinators as cmb
    # FIXME do not want circular imports incoming ...
    from pyontutils.namespaces import TEMP, OntCuries
    from pyontutils.namespaces import prot, proc, tech, asp, dim, unit, ilxtr
    from pyontutils.closed_namespaces import rdf, owl, rdfs
    xsd = rdflib.XSD
    OntCuries({'unit':str(unit)})
except ImportError:
    pass  # exception logged in rdftypes


ur = pint.UnitRegistry()
ur.preprocessors.append(lambda s: s.replace('%', 'percent'))
ur.load_definitions((Path(__file__).parent / 'pyr_units.txt').as_posix())
ur.default_system = 'mgs'  # SNAAAKKEEEEE system
pint.set_application_registry(ur)


class __Monkey:

    def __hash__(self):
        """ monkey patch so that +/- can go in a dict """
        return hash((self.__class__, self._nominal_value, self._linear_part))

    def __eq__(self, other):
        return (type(self) == type(other) and
                self.value == other.value and
                self.error == other.error)

    def __lt__(self, other):
        max_vs = self.value + self.error
        if type(self) == type(other):
            min_vo = other.value - other.error
        else:
            min_vo = other

        # our max is less than their min
        return max_vs < min_vo

    def __le__(self, other):
        return self < other or self == other

    def __gt__(self, other):
        min_vs = self.value - self.error
        if type(self) == type(other):
            max_vo = other.value + other.error
        else:
            max_vo = other

        # our min is greater than their max
        return min_vs > max_vo

    def __ge__(self, other):
        return self < other or self == other


AffineScalarFunc.__hash__ = __Monkey.__hash__
ur.Measurement.__eq__ = __Monkey.__eq__
ur.Measurement.__lt__ = __Monkey.__lt__
ur.Measurement.__gt__ = __Monkey.__le__
ur.Measurement.__le__ = __Monkey.__gt__
ur.Measurement.__ge__ = __Monkey.__ge__


_deprecated_units = (
    # used to prevent use of deprecated units as derived units
    'lambda',
)


try:
    _pintuc = pint.unit.UnitsContainer
except AttributeError:
    # horray for absolutely horribly maintenace practices
    _pintuc = pint.util.UnitsContainer


class _Unit(intf.Unit, ur.Unit):

    def format_babel(self, spec='', **kwspec):
        spec = spec or self.default_format

        if '~' in spec:
            #if self.dimensionless:
                #return ''
            units = _pintuc(dict((self._REGISTRY._get_symbol(key),
                                  value)
                                 for key, value in self._units.items()))
            spec = spec.replace('~', '')
        else:
            units = self._units

        return '%s' % (units.format_babel(spec, **kwspec))

    def json(self, ld=True):
        # can't use asDerived in here, only
        # the quantity should do that
        out = str(self)
        if ld:  # FIXME compound units are annoying
            out = quote(str(self), safe=tuple())

        return out

    def asRdf(self):
        return unit[quote(str(self), safe=tuple())]

    @property
    def _name(self):
        return next(iter(self._units._d))

    def asDerived(self):
        """ return a derived unit if one exists for the current units """
        if self.dimensionality:
            try:
                if '[time]' in self.dimensionality:
                    # don't rewrite time units here
                    return
                elif '[temperature]' in self.dimensionality:
                    # don't rewrite temp due to issues with
                    # absolute units being non multiplicative
                    # and having to use e.g. delta_degC
                    return

                deru = [u for u in self.compatible_units()
                        if u._name not in _deprecated_units and
                        self.compare(u, operator.eq) and
                        u != self and
                        # Hz & friends cause issues because the denominator is typed
                        (1 * u).to_base_units() == (1 * self).to_base_units() and
                        # TODO u.systems?
                        len(str(u)) < len(str(self))]
                if deru:
                    return deru[0]  # FIXME what to do in cases where > 1 ?
            except KeyError as e:
                # cannot be simplified
                # PS how is it that density does not have a derived unit !?
                pass

    def __reduce__(self):
        f, (unit, container) = super().__reduce__()
        return pint._unpickle_unit, (ur.Unit, container)


class _PrefixUnit(_Unit):
    pass


class _Quant(intf.Quantity, ur.Quantity):

    def to_base_units(self):
        """Return Quantity rescaled to base units
        """
        _, other = self._REGISTRY._get_base_units(self._units)

        magnitude = self._convert_magnitude_not_inplace(other)

        return self.__class__(magnitude, other)

    #@property
    #def magnitude(self):
        #magnitude = super().magnitude
        #return magnitude

    def json(self):
        # FIXME prefix vs suffix quantities
        derq = self.asDerived()
        if derq:
            return derq.json()

        return dict(type=self.tag, magnitude=self.magnitude, units=self.units.json())

    @classmethod
    def fromJson(cls, json):
        assert json['type'] == cls.tag
        try:
            return cls(json['magnitude'], json['units'])
        except KeyError:
            # support old naming convention
            return cls(json['value'], json['unit'])
        except ValueError as e:
            if '%' in json['units']:
                return cls(json['magnitude'], unquote(json['units']))
            else:
                raise e

    def asRdf(self, subject, rdftype=None):
        """ to asRdf the normalized units use q.to_base_units().asRdf(s)"""
        if rdftype is None:
            rdftype = ilxtr.Quantity

        magnitude = TypeCaster.cast(self.magnitude)
        yield subject, rdf.type, rdftype
        if not self.units:  # FIXME ... predicate how?
            yield subject, rdf.value, magnitude.asRdf()
            return

        #value, unit = self.units.asRdf(self.magnitude)
        yield subject, rdf.value, magnitude.asRdf()
        yield subject, TEMP.hasUnit, self.units.asRdf()

    def asDerived(self):
        if self.dimensionality:
            deru = self.units.asDerived()
            if deru:
                return self.magnitude * deru

    @property
    def ttl(self):
        graph = rdflib.Graph()
        OntCuries.populate(graph)
        [graph.add(t) for t in self.asRdf(rdflib.BNode())]
        return graph.serialize(format='nifttl')

    def __reduce__(self):
        f, (quant, mag, *units) = super().__reduce__()
        return pint._unpickle_quantity, (ur.Quantity, mag, *units)


class _Measurement(intf.Measurement, _Quant, ur.Measurement):

    def json(self):
        # FIXME prefix vs suffix quantities
        derq = self.asDerived()
        if derq:
            return derq.json()

        return dict(type=self.tag,
                    value=self.value.magnitude,
                    error=self.error.magnitude,
                    units=self.units.json())

    @classmethod
    def fromJson(cls, json):
        # error value unit
        assert json['type'] == cls.tag
        return cls(json['value'],
                   json['error'],
                   json['units'])

    def asRdf(self):
        raise NotImplementedError('TODO')

    def __reduce__(self):
        f, (quant, value, error, *units) = super().__reduce__()
        return pint._unpickle_measurement, (ur.Measurement, value, error, *units)


ur.Unit = _Unit
ur.Quantity = _Quant
ur.Measurement = _Measurement

class RacketNumber(Number):

    def __new__(cls, number):
        if not isinstance(number, Number):
            raise TypeError(f'wat {type(number)}')

        self = super().__new__(cls)
        self._number = number
        return self

    def asPython(self):
        return self._number

    def __eq__(self, other):
        return type(self) == type(other) and self._number == other._number

    def __repr__(self):
        return f'RacketNumber<{self._number!r}>'

    def __getnewargs_ex__(self):
        return (self._number,), {}


class RacketString(str):

    @property
    def prov(self):
        if not hasattr(self, '_prov'):
            if 'protc: https://hyp.is/' in self:
                _, id_rest = self.split('protc: https://hyp.is/', 1)
                id, rest = id_rest.split(' ', 1)
                return Hyp._HypothesisAnno(id)  # simplify config
            else:
                self._prov = None

        return self._prov

    def asPython(self):
        return self

    def asJson(self):
        return str(self)


class SExpr(tuple):
    """ This class can act as a pretty formatter for s-exprs or it
        can act as a pretty printing object. To use it as a formatter
        initialize it without any arguments as spp = SexpPrettyPrinter()
    """

    format_nl =  '*', '/', 'range', 'plus-or-minus', 'param:dimensions'
    format_nl_long =  '^',  # the empty string is in all strings, so have to use tuple

    def __call__(self, sexp, indent=0):
        return self.__repr__(indent, sexp)

    @classmethod
    def isLongNL(cls, sexp):
        # and right here is where you want let
        # you could skip this branch entirely except that
        # it is much harder to read when you can't assign
        # t1 and t2 but you have to assign them after the branch
        if sexp[0] not in cls.format_nl_long:
            return

        _op, s1, s2 = sexp
        t1 = type(s1) is tuple
        t2 = type(s2) is tuple
        # have to defer the call to len until after the type check
        return (t1 and t2 and (len(s1) > 2 or len(s2) > 2) or
                t1 and len(s1) > 3 or
                t2 and len(s2) > 3)

        if sexp[0] in cls.format_nl_long:
            t1 = type(sexp[1]) is tuple
            t2 = type(sexp[2]) is tuple
            if t1 and t2:
                if len(sexp[1]) > 2 or len(sexp[2]) > 2:
                    return True
            elif t1 and len(sexp[1]) > 3:
                return True
            elif t2 and len(sexp[2]) > 3:
                return True
        return False

    @classmethod
    def format_value(cls, sexp, localIndent=0, depth=0, SPACE=' '):
        out = ''
        if sexp:
            if sexp[0] == 'quote':  # TODO other quasiquote etc.
                v = sexp[1]

                if isinstance(v, tuple):
                    out = cls.format_value(v)
                else:
                    out = v

                return "'" + out

            newline = sexp[0] in cls.format_nl or cls.isLongNL(sexp)
            indent_for_this_loop = localIndent + len('(') + len(sexp[0]) + len(' ')  # vim fail )
            indent_for_next_level = indent_for_this_loop
            for i, element in enumerate(sexp):
                if newline and i > 1:
                    out += '\n' + SPACE * indent_for_this_loop

                if isinstance(element, tuple):
                    element = cls.format_value(element, indent_for_next_level, depth + 1)

                if element is not None:
                    strelement = str(element)
                    if out and out[-1] != SPACE:
                        out += ' ' + strelement
                        if i > 1 or not newline:
                            # unlike at the top strelement already has ( prepended if it exists
                            indent_for_next_level += len(SPACE) + len(strelement)

                    else:  # we are adding indents
                        out += strelement
        if out:
            return '(' + out + ')'

    def _repr_pretty_(self, p, cycle):
        """ needed by pprint """
        if cycle:
            p.text(self.__class__.__name__ + '(CYCLE...)')
        else:
            indent = p.output_width
            p.text(self.__repr__(indent))

    def __repr__(self, indent=0, sexp=None):
        cname = self.__class__.__name__
        if sexp is None:
            sexp = self
        formatted = self.format_value(sexp, len(cname) + 1 + indent, 0) 
        return cname + f'({formatted})'


class Oper(Expr):
    """ Operators """


class LoR(Oper):
    op = None
    # TODO operator precidence
    def __init__(self, left, right):
        #if [e for e in (left, right) if isinstance(e, UnitSuffix)]:
            #raise TypeError(f'{left!r} {right!r}')

        #if type(left) != type(right):  # oper vs atomic is what primarily fails this
            #raise TypeError(f'{left!r} {right!r}')

        self.left = left
        self.right = right

    def __hash__(self):
        return hash((self.__class__, self.left, self.right))

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.left == other.left and
                self.right == other.right)

    #@property
    #def simplified(self):
        #l = self.left
        #r = self.right
        #if isinstance(l, Quantity):
            #value_ = getattr(l.value, self._op)(r.value)
            #unit__ = getattr(l.unit, self._op)(r.unit)
        #else:
            #unit = self
        #value, unit_ = unit__(value_)
        #breakpoint()
        #return value_, unit__

    #def __str__(self):
        #value = self.value
        #value = '' if value is None else value
        #return f'{value}{self.unit}'
        #return f'{self.left}{self.op}{self.right}'

    def __repr__(self):
        return f'{self.__class__.__name__}({self.left!r}, {self.right!r})'

    #@property
    #def value(self):
        #l = self.left
        #r = self.right
        #if not isinstance(l, Quantity):
            #return None

        #return getattr(l.value, self._op)(r.value)  # FIXME normalize wrt units ...

    #@property
    #def unit(self):
        #l = self.left
        #r = self.right
        # if we are maximally reduced ...
        #return (getattr(l.unit, self._op)(r.unit)
                #if l.unit and r.unit
                #else (l.unit if l.unit
                      #else (r.unit if r.unit else Unit(None, None))))

    @property
    def prefix(self):
        return self.unit.prefix

    def reduce(self):
        # FIXME naming ...
        value = self.value
        if self.value is None:
            return self.unit


class Add(LoR):
    op = '+'
    _op = '__add__'


class Mul(LoR):
    op = '*'
    _op = '__mul__'


class Div(LoR):
    op = '/'
    _op = '__truediv__'


class Exp(LoR):
    op = '^'
    _op = '__pow__'


Expr.bindImpl(None, Add, Mul, Div, Exp)


class SIPrefix(UnitsHelper, strc):
    def __new__(cls, prefix):
        if prefix is None:
            prefix = ''

        return super().__new__(cls, prefix)

    # TODO allow init from short or from long
    def __truediv__(self, other):
        return self.prefix(self.exponent - other.exponent)  # FIXME ... -?

    def __mul__(self, other):
        return self.prefix(self.exponent + other.exponent)

    @property
    def exponent(self):
        return self.si_exponents[self.fullName if self else None]

    def prefix(self, exponent):
        # FIXME this requries the ability to instantiate from long name
        return self.__class__(self.si_exponents_inv[exponent])

    def to_base(self, prefixed_value):
        """ return a value in this unit in the base unit """
        return prefixed_value * 10 ** self.exponent

    def from_base(self, base_value):
        # FIXME come up with clearer naming
        return base_value / 10 ** self.exponent

    @property
    def fullName(self):
        # these should always be known
        if not self:
            return self

        if self in self.prefix_dict:
            return self
        else:
            return self.prefix_dict_inv[self]


class UnitSuffix(UnitsHelper, strc):  # not an expression, an atom

    # FIXME if we do this it should really be intc ... and be 1 ...

    def __new__(cls, suffix):
        if suffix is None:
            suffix = ''

        return super().__new__(cls, suffix)

    @property
    def prefix(self):
        return SIPrefix(None)

    @property
    def unit(self):
        return strc(self)

    @property
    def fullName(self):
        try:
            return self.unit_dict_inv[self]
        except KeyError:
            try:
                return self.unit_dict[self]
            except KeyError:
                return  self.__class__('') # FIXME

    def __truediv__(self, other):
        # FIXME do we really want to implement each unit as
        # its own subclass here? no? I think not? use the lu table?
        # no objects all the way down :/

        # FIXME as pposed to having Mul Div etc all handle this for all types???
        # I think this makes more sense this way
        return self._lu('/', '__truediv__', other)

    def __mul__(self, other):
        # slowly progressing to more elegent ...
        if not self:
            return other  # (* 1)  ??

        return self._lu('*', '__mul__', other)

    def _lu(self, op, _op, other):
        lut = self.conversion[_op]
        fs = frozenset((self, other))
        if fs in lut:
            return lut[fs]  # TODO Unit(lut[fs]) ?
        else:
            # cannot reduce
            return Unit(f'{self}{op}{other}')

    def __repr__(self):
        return f'{self.__class__.__name__} <{self}>'


class Unit(Expr):

    full = ('weeks', 'days', 'months', 'years')

    derived = {}
    base = {}

    def __init__(self, unit_symbol, prefix=None):
        if unit_symbol in self.derived:
            derived_unit = unit_symbol
            base_unit = self.derived[derived_unit]
        elif unit_symbol in self.base:
            base_unit = base_unit
        else:
            base_unit = None

        if not isinstance(base_unit, UnitSuffix):
            base_unit = UnitSuffix(base_unit)

        if not isinstance(prefix, SIPrefix):
            prefix = SIPrefix(prefix)

        self.prefix = prefix
        self.base_unit = base_unit

    @property
    def suffix(self):
        return self.derived_unit if self.derived_unit else self.base_unit

    @property
    def fullName(self):
        return self.prefix.fullName + self.suffix.fullName

    def __str__(self):
        prefix = self.prefix if self.prefix else ''
        unit = (' ' + self.suffix.fullName
                if self.suffix.fullName in self.full
                else self.unit)

        return f'{prefix}{unit}'

    def __repr__(self):
        return f'{self.__class__.__name__} <{self}>'

    def __mul__(self, other):
        return self._Mul(self, other)
        #return self.__class__(f'{self}*{other}')

    def __imul__(self, other):
        return self.__mul__(other)

    def __rtruediv__(self, other):
        return self._Div(self, other)
        #return self.__class__(f'{self}/{other}')

    def __pow__(self, other):
        return self._Exp(self, other)
        #return self.__class__(f'{self}^{other}')

    def __truediv__(self, other):
        # TODO do the unit math ...
        return self._Div(self, other)  # hah, preserve composability
        #return self.__class__(f'{self}/{other}')

    def __eq__(self, other):
        # TODO do the unit math
        return self.unit == other.unit and self.prefix == other.prefix

    def __hash__(self):
        return hash((hash(self.__class__), self.unit, self.prefix))

    def json(self):
        return str(self)

    def asRdf(self, value):
        base_unit = self.unit.fullName.asRdf()
        # FIXME this would be so much easier if i just
        # implemented everything in one place ...
        return rdflib.Literal(self.prefix.to_base(value)), base_unit


class PrefixUnit(Unit):
    """ There are some use cases for the unit also carrying this information """

    def __init__(self, unit, prefix=None):
        if isinstance(unit, Unit):
            super().__init__(unit.unit, unit.prefix)
        else:
            super().__init__(unit, prefix)


class __Quantity(Expr):

    tag_suffix = 'quantity'

    def __init__(self, value, unit=None):
        self.value = TypeCaster.cast(value)
        self.unit = unit

    def __str__(self):
        unit = self.unit if self.unit else ''
        return f'{self.value}{unit}'

    def __repr__(self):
        unit = f', {self.unit!r}' if self.unit else ''
        return f'{self.__class__.__name__}({self.value!r}{unit})'

    def __eq__(self, other):
        # TODO do the unit math
        #print(repr(self), repr(other))
        return self.value == other.value and self.unit == other.unit

    def __mul__(self, other):
        # a unitless 'quantity' is given units by multiplication
        # rather than by addition (duh) given the nice algebraic
        # properties that that gives the resulting unit

        if isinstance(other, Unit) and self.unit is None:
            return self.__class__(self.value, other)

        else:
            return super().__mul__(other)

    def __add__(self, other):
        if isinstance(other, Quantity) and self.unit.unit == other.unit.unit:
            sv = self.to_base()
            ov = other.to_base()
            nv = self.unit.prefix.from_base(sv + ov)  # TODO check this is correct
            return self.__class__(nv, self.unit)
        else:
            raise TypeError(f'{self!r} + {other!r} failed because other is a {type(other)}')

    def to_base(self):
        return self.prefix.to_base(self.value)

    def prefix(self):
        if self.unit:
            return self.unit.prefix

        return SIPrefix(None)

    def json(self):
        return dict(type=self.tag_suffix, value=self.value, unit=self.unit)

    #@express
    def asRdf(self, subject):
        if not self.unit:  # FIXME ... predicate how?
            yield subject, TEMP.hasValue, self.value.asRdf
            return

        value, unit = self.unit.asRdf(self.value)
        yield subject, TEMP.hasValue, value
        yield subject, TEMP.hasUnit, unit


class __PrefixQuantity(__Quantity):

    tag_suffix = 'prefix-quantity'

    def __repr__(self):
        unit = f', {self.unit!r}' if self.unit else ''
        return f'{self.__class__.__name__}({self.value!r}{unit})'

    def __str__(self):
        unit = self.unit if self.unit else ''  # this case shouldn't happen but better safe than sorry
        return f'{unit}{self.value}'


class Range(intf.Range, Oper):
    """ This is a non-homogenous range, units may differ """
    op = '-'

    def asDerived(self):
        deru = self.units.asDerived()
        if deru:
            return self.__class__(
             *(_.asDerived() for _ in (self.left, self.right)))

    def compatible_units(self, *contexts):
        return self.units.compatible_units(*contexts)

    def to_base_units(self):
        return self.__class__(
            *(_.to_base_units() for _ in (self.left, self.right)))

    @property
    def dimensionality(self):
        ld = self.left.dimensionality
        rd = self.right.dimensionality
        if ld != rd:  # FIXME should catch during construction ya?
            msg = (f'{self.__class__.__name__} dimensionality '
                   f'mismatch! {ld} != {rd}')
            raise TypeError(msg)

        return ld

    @property
    def units(self):
        if self.left.units == self.right.units:
            return self.left.units
        else:
            msg = ('The current range uses non-homogenous range units '
                   'use to_base_units first and then you can get units. '
                   'Alternately, depending on your use case you could use '
                   'dimensionality.')
            raise NotImplementedError(msg)

    def __init__(self, left, right):
        self.left = left
        self.right = right
        # unit representation is dealt with by the parser
        # range could figure it out now with the info
        # provided, but for now is just going to be a dumb
        # container

    def __hash__(self):
        return hash((self.__class__,
                     self.left,
                     self.right,))

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.left == other.left and
                self.right == other.right)

    def __gt__(self, other):
        if type(self) == type(other):
            return not self < other
        else:
            return True  # range always wants to be highest

    def __ge__(self, other):
        return self > other or self == other

    def __lt__(self, other):
        if type(self) == type(other):
            return ((self.left - self.right) ** 2 <
                    (other.left - other.right) ** 2)
        else:
            return False

    def __le__(self, other):
        return self < other or self == other

    def __mul__(self, other):
        # FIXME rmul on units?
        if isinstance(other, _Unit):
            return self.__class__(self.left * other, self.right * other)
        else:
            raise NotImplementedError

    def _apply_nonmult_units(self, unit):
        return self.__class__(ur.Quantity(self.left, unit), ur.Quantity(self.right, unit))

    def __str__(self):
        return f'{self.left}{self.op}{self.right}'

    def __repr__(self):
        return f'{self.__class__.__name__}({self.left!r}, {self.right!r})'

    def nist(self):
       #see_also = ('https://www.nist.gov/pml/nist-guide-si-chapter-7-rules-and-style-'
                   #'conventions-expressing-values-quantities#77 ') 

        return f'{self.left} to {self.right}'

    def json(self):
        return dict(type=self.tag,
                    start=self.left.json(),
                    stop=self.right.json())

    @classmethod
    def fromJson(cls, json):
        assert json['type'] == cls.tag
        startj = json['start']
        stopj = json['stop']
        # TODO need a registry of these
        if startj['type'] == 'quantity':
            start = ur.Quantity.fromJson(startj)
        else:
            raise ValueError(startj)

        if stopj['type'] == 'quantity':
            stop = ur.Quantity.fromJson(stopj)
        else:
            raise ValueError(stopj)

        return cls(start, stop)

    def asRdf(self, subject=None, quantity_rdftype=None):
        # FIXME what to do about quantity_rdftype??

        # TODO correctly done inside a restriction as well
        if subject is None:
            subject = rdflib.BNode()

        nl = self.left.to_base_units()
        nr = self.right.to_base_units()

        nlm = TypeCaster.cast(nl.magnitude)
        nrm = TypeCaster.cast(nr.magnitude)

        #left = self.left.magnitude
        #right = self.right.magnitude
        type_ = (xsd.integer if
                 isinstance(nl, int) and
                 isinstance(nr, int)
                 else xsd.real)  # FIXME owl:real isn't in the namespace but people use it anyway?

        if nl.units:
            type_ = nl.units.asRdf()
        elif nr.units:
            # this case shouldn't happen since the
            # interpreter normalizes range units before we get here
            type_ = nr.units.asRdf()

        def min_(s, p):
            o = rdflib.BNode()
            yield s, p, o
            yield o, xsd.minInclusive, nlm.asRdf()

        def max_(s, p):
            o = rdflib.BNode()
            yield s, p, o
            yield o, xsd.maxInclusive, nrm.asRdf()

        yield subject, rdf.type, rdfs.Datatype
        yield subject, owl.onDatatype, type_
        yield from cmb.olist.serialize(subject, owl.withRestrictions, min_, max_)


class PlusOrMinus(Range):
    op = _plus_or_minus
    tag = 'plus-or-minus'


class Ratio(LoR):  # FIXME dilution is an aspect where the value is a ratio

    # TODO parsing of ratios vs times
    # includes mass ratio
    # 1 : dilution-factor
    # snr etc.
    # XXX NOTE the referents must also be
    # arranged in a left:right pattern
    # usual (aspect thing-1) : (aspect thing-2) :: number-1 : number-2

    op = ':'
    tag = 'ratio'
    dimensionality = pint.util.UnitsContainer({'[]': 1.0})

    def json(self):
        return dict(type=self.tag, left=self.left, right=self.right)

    def fromJson(cls, json):
        assert json['type'] == cls.tag
        return cls(json['left'], json['right'])

    def __str__(self):
        return self.op.join((str(_) for _ in (self.left, self.right)))

    def __gt__(self, other):
        # a larger dilution is a smaller fraction (confusingly)
        if type(self) == type(other):
            return self.right / self.left > other.right / other.left
        else:
            return True


class Dimensions(Oper):
    op = 'x'
    tag = 'dimensions'

    @property
    def dimensionality(self):
        value = reduce(operator.mul, self.quants)
        return value.dimensionality

    def __init__(self, *quants):
        self.quants = quants

    def __str__(self):
        return f' {self.op} '.join(v.format_babel('~', locale='en_US') for v in self.quants)

    def __repr__(self):
        return str(self)

    def asRdf(self):
        self.dims  # FIXME name error incoming?

    def json(self):
        return dict(type=self.tag, value=self.quants)

    def fromJson(cls, json):
        assert json['type'] == cls.tag
        return cls(*json['value'])


class Approximately(Oper):
    """ No numercial uncertainty given, but with facilities to operationalize it. """

    op = '~'
    tag = 'approximately'

    @property
    def dimensionality(self):
        # FIXME this will come back to bite us I suspect
        return self.expr.dimensionality

    @property
    def units(self):
        return self.expr.units

    def __init__(self, expr):
        # FIXME accepting an already parsed value causes
        # inversion issues when coming from json
        self.expr = expr

    def __str__(self):
        return f'~{self.expr}'

    def __repr__(self):
        return f'{self.__class__.__name__}({self.expr!r})'

    def quantify(self, error, relative=False):
        """ provide concrete values for the approximateness of approximately """
        return self.expr.plus_minus(error, relative)

    @classmethod
    def fromJson(cls, json):
        assert json['type'] == cls.tag
        # FIXME the generic fromJson from sparcur doesn't work here
        value = json['value']
        for c in (ur.Quantity, Ratio, Dimensions):
            if c.tag == value['type']:
                return cls(c.fromJson(value))
        else:
            # FIXME this will come back to bite us
            msg = f'How to approximate {value["type"]}'
            raise NotImplementedError(msg)

    def json(self, ld=True):
        out = str(self)  # FIXME not the best decision here
        if ld:  # FIXME compound units are annoying
            #out = quote(str(self), safe=tuple())
            # FIXME TODO not entirely clear what order these go in if
            # you are thinking with types then you would want to know
            # that the value was a quantity first and then that the
            # value itself was approximate not that the units or
            # anything else were approximate, pint doesn't do this
            # and instead has a separate class Measurement that can
            # manage uncertainty, but it has to be quantified, not
            # approximate

            # XXX NOTE practically we keep Approximately outside in
            # the python implementation because doing the right thing
            # and creating an approximate magnitude that would work
            # correctly inside a pint quantity would mean having a way
            # to work with unquantified pint measurements, which
            # requires stronger mathematic machinery than pint or pyr
            # has right now, and would require propagating the types
            # in the approximateness through whole equations which is
            # no fun for a variety of reasons, having a default way to
            # determine the +/- isn't really viable, the user needs to
            # provide a concrete estimate, because ~1000 -> +/- 10
            # is dramatically different than ~1000 +/- 100 vs +/- 500

            # the trade off is whether you want to deal with
            # approximate values wherever quantities might appear in
            # the ast, or whether you want to deal with approximate
            # values anywhere that you might ever encounter an number
            # ugh approx is extremely ambiguous ...

            # TODO jsonld bits
            out = {'type': self.tag,
                   'value': self.expr.json()}

        return out

    def __hash__(self):
        return hash((self.__class__, self.expr))

    def __eq__(self, other):
        # only approximate values can be equal to eachother
        # any additional quantification of uncertainty
        # they should probably be ranked by their
        # level of uncertainty
        return (type(self) == type(other) and
                self.expr == other.expr)

    def __gt__(self, other):
        return (self.expr > other.expr
                if type(self) == type(other) else
                (self.expr >= other  # equality -> gt, see tests for notes
                 if type(self.expr) == type(other) or isinstance(other, Number)
                 else True))
        # NOTE approximate values are always worst case
        # in the case of equality, when you want to know
        # if they are greater, they are, when you want
        # to know if they are less than, they are

    def __ge__(self, other):
        return self > other or self == other

    def __lt__(self, other):
        return (self.expr < other.expr
                if type(self) == type(other) else
                (self.expr <= other  # equality -> le, see tests for notes
                 if type(self.expr) == type(other) or isinstance(other, Number)
                 # need to test against Number for compat with pint Quantity
                 else True))

    def __le__(self, other):
        return self < other or self == other


class Iso8601Duration(intf.AJ):
    """ There is no easy way to work with these, these durations are in
        many cases invariants and NOT parameters because the duration of
        the components of a date (day, month, year) are variable depending
        on the exact start time. XXX TODO It might be worth considering
        parsing P and PT differently as a result, because PT durations are
        always of known duration, whereas the duration of P is not. Basically
        date intervals are invariants, time intervals are parameters, and
        datetime intervals are also invariants with some potentially weird
        behavior depending on the exact order in which the component durations
        are resolved. """

    _order = ('year',
              'month',
              'day',
              'hour',
              'minute',
              'second',)

    _isoletter = {'year': 'Y',
                  'month': 'M',
                  'day': 'D',
                  'hour': 'H',
                  'minute': 'M',
                  'second': 'S',}

    def __init__(self, *quantities, prov='TODO'):
        self.quantities = quantities
        self.prov = prov

    def _value_qs(self):
        def key(q):
            su = str(q.units)
            if su in self._order:
                return 0, self._order.index(su)
            else:
                # FIXME XXX should probably be an error
                return 1, su

        first_time = True
        def mak(q):
            su = str(q.units)
            out = f'{q.magnitude}{self._isoletter[su]}'
            nonlocal first_time
            if first_time and su in ('hour', 'minute', 'second'):
                first_time = False
                return 'T' + out

            return out

        return 'P' + ''.join(
            [mak(q) for q in sorted(self.quantities, key=key)])

    @property
    def _value(self):
        value = self._value_qs()
        return value


class Iso8601DurationTime(Iso8601Duration):

    _type = 'iso8601-duration-time'
    tag = _type

    def __init__(self, *quantities):
        super().__init__(*quantities)
        self.duration = sum(quantities)


class Iso8601DurationDate(Iso8601Duration):

    _type = 'iso8601-duration-date'
    tag = _type

    def from_beg(self, datetime_start):
        # XXX FIXME yeah, P and PT should be handled separately
        raise NotImplementedError('TODO')

    def from_end(self, datetime_end):
        raise NotImplementedError('TODO')


class Iso8601DurationDatetime(Iso8601DurationDate):

    _type = 'iso8601-duration-datetime'
    tag = _type


class Quote(SExpr):
    pass


class mode(Enum):
    FAIL = 1
    WARN = 2


class MacroDecorator:
    """ define functions in order to get order! """
    def __init__(self):
        self.macros = tuple()

    def has_macros(self, cls):
        if not hasattr(cls, '_macros'):
            cls._macros = self.macros
        else:
            cls._macros += self.macros

        return cls

    def __call__(self, function):
        if function.__name__ not in self.macros:
            self.macros += function.__name__,
            function._is_macro = True  # more efficient to lookup this way
        else:
            raise ValueError(f'Duplicate function name {function.__name__}')

        return function


class Environment:
    def __init__(self, namespaces=None, **kwargs):
        if namespaces is None:
            namespaces = {}

        self.namespaces = namespaces
        [setattr(self, k, v) for k, v in kwargs.items()]


macro = MacroDecorator()
@macro.has_macros
class Interpreter:

    class _config:  # FIXME ... bad way to do this
        data_failure_mode = mode.FAIL
        parser_failure_mode = mode.FAIL

    _renames = {
        '+': 'plus',
        '-': 'minus',
        '*': 'multiplication',  # FIXME unit vs normal implies macro
        '/': 'division',
        '^': 'exp',
        '<': 'less_than',
        '>': 'greater_than',
        '<=': 'less_than_or_equal',
        '>=': 'greater_than_or_equal',
    }

    _keyword_start = '#:'  # FIXME way too hardcoded here

    def __init__(self, environment=Environment()):
        self.environment = environment
        self.pretty_printer = SExpr()

        self._namespaces = self.environment.namespaces
        if self.namespace not in self._namespaces:
            self._namespaces[self.namespace] = self

        [c(environment) for n, c in self._namespace_classes.items()
         if c.namespace not in self._namespaces]

    def lisp_to_python(self, lisp_identifier):
        if lisp_identifier in self._renames:
            return self, self._renames[lisp_identifier]

        *namespace, name = lisp_identifier.split(':', 1)
        if not namespace:
            namespace = self  # unprefixed can be defined locally or on the Interperter base class
        else:
            namespace, = namespace
            namespace = namespace.replace('-', '_')
            namespace = self._namespaces[namespace]

        return namespace, name.replace('-', '_')

    def __call__(self, sexp):
        python_repr = self.eval(sexp)  # raises exc.ParseFailure

        try:
            python_repr._sexp = sexp
        except AttributeError:
            # if we eval to a python literal they cannot accept the _sexp
            pass

        return python_repr

    def pretty_print(self, expression):
        if isinstance(expression, tuple):
            # this is the 'reusable' version
            print(self.pretty_printer(expression))
        else:
            print(expression)

    def eval(self, expression):
        if isinstance(expression, tuple) or isinstance(expression, list):
            # tuple unpacking produces lists because generators
            # have unkown lenght
            tup = expression
            if not tup:
                return None
        else:
            if isinstance(expression, str) and 'protc: ' in expression:
                # FIXME this is a horrible evil hack around how I implemented
                # skipping nodes that I didn't know what to do with in protc/ur
                # I think there just need to be an unhanded node
                return RacketString(expression)

            return expression

        first, *rest = tup
        namespace, pyfirst = self.lisp_to_python(first)
        try:
            function_or_macro = getattr(namespace, pyfirst)
        except AttributeError as e:
            # FIXME parsing here strings work but the -> python repr is givign issues in here
            raise ValueError(tup) from e

        if (hasattr(function_or_macro, '_is_macro') and function_or_macro._is_macro):
            return function_or_macro(*rest)
        elif namespace.__class__ != self.__class__:
            # this is so that e.g. division can be defined internal to some namespace an not exported i.e. not explicilty prefixed
            # it is NOT a good way to do this at all but I think it works
            # FIXME TODO need the need the stack of environments
            return namespace.eval(expression)

        if isinstance(rest, list) or isinstance(rest, tuple):
            args = []
            kwargs = {}
            gen = (_ for _ in rest)
            while True:
                try:
                    r = next(gen)
                    if isinstance(r, str) and r.startswith(self._keyword_start):
                        # and this is why you have an ast ... with a keyword aware reader
                        key = r[2:].replace('-', '_')  # FIXME lisp ids can get quite fancy
                        if key in kwargs:
                            raise ValueError(f'keyword argument {r} supplied twice!')

                        r = next(gen)
                        kwargs[key] = self.eval(r)
                    else:
                        args.append(self.eval(r))

                except StopIteration:
                    break

        try:
            return function_or_macro(*args, **kwargs)  # apply is * woo
        except TypeError as e:
            raise ValueError(f'{args} {kwargs}') from e

    @macro  #duh
    def quote(self, expression):
        if isinstance(expression, tuple):
            return Quote(('quote', expression))  # TODO improve the quote class a bit probably ...

        # self evaluating expressions ?
        # string vs symbol not resolved here?
        out = self.eval(expression)
        if out != expression:  # hilariously expensive check right here
            raise ValueError('something went wrong {expression} != {out}')

        return out

    def rest(self, expression):
        return ('rest', expression)

    def iso8601_duration_time(self, *quantities):
        return Iso8601DurationTime(*quantities)

    def iso8601_duration_datetime(self, *quantities):
        return Iso8601DurationDatetime(*quantities)

    def iso8601_duration_date(self, *quantities):
        return Iso8601DurationDate(*quantities)


macro = MacroDecorator()
@macro.has_macros
class ParamParser(UnitsHelper, ImplFactoryHelper, Interpreter):
    """ definitions for the param: namespace """

    namespace = 'param'

    _Unit = None
    _PrefixUnit = None
    _Quantity = None
    _PrefixQuantity = None
    _Range = None
    _Dilution = None
    _Dimensions = None

    def parse_failure(self, *args, node_type=None, failed_input=None, prov=None):
        if prov:
            # prov in this case just is the protc node
            e = exc.ParseFailure(prov)
        else:
            e = exc.ParseFailure(node_type, failed_input)

        if self._config.parser_failure_mode == mode.FAIL:
            raise e

        return e

    def expr(self, sexp):
        return sexp

    def unit_expr(self, expression):  # TODO probably a macro
        return expression

    @macro  # oops, sometime unit expressions are hidden in units :x
    def unit(self, unit, prefix=None):
        if unit[0] == '/':  # FIXME broken parser output
            return self.eval(unit)

        quoted_unit = self.eval(unit)
        if prefix is not None:
            prefix = self.eval(prefix)

        unquoted_unit = quoted_unit
        #unquoted_unit = quoted_unit[1:]
        p = self._prefix(prefix)
        u = self._unit(unquoted_unit)

        pu = p + u
        try:
            urpu = ur.parse_units(pu)
        except BaseException as e:
            raise ValueError(pu) from e

        return urpu

    def _prefix(self, prefix):
        if prefix:
            return prefix
            #return self.prefix_dict[prefix]
        else:
            return ''  # FIXME pint PrefixDefinition?

    def _unit(self, unquoted_unit):
        # don't bother to shorten, we will do that at rendering time
        # do replace the - with _ to help pint
        return unquoted_unit.replace('-', '_')

    def prefix_unit(self, unit):
        return _PrefixUnit(self.unit(unit))  # FIXME need a way to mark these ...
        #return self._PrefixUnit(self.unit(unit))

    #@macro(True, False)  # TODO specify which values can be evaluated
    @macro
    def quantity(self, value, unit=None):
        """ FIXME this prefix_unit issue reveals that this should
            really be prefix-quantity so that it doesn't have to
            be a macro that looks for a prefix-unit """

        # FIXME range masquerading as a quantity

        if isinstance(value, str):
            breakpoint()
        value = self.eval(value)
        value = value if value else 1  # multiplication by 1 for units if the unit is None we get zero?
        unit_value = self.eval(unit)
        # the null unit is just null so we have to handle that since it has no type
        if unit_value is None:
            unit_value = ur.dimensionless

        if unit_value.dimensionality == '[temperature]':
            if isinstance(value, Range):
                return value._apply_nonmult_units(unit_value)
            else:
                return ur.Quantity(value, unit_value)  # ah, just set it to 1 for no units ... fun
        else:
            return value * unit_value  # ah, just set it to 1 for no units ... fun

    def plus(self, *operands):
        first, *rest = operands
        for o in rest:
            first += o

        return first

    def minus(self, left, right):
        return left - right
        #return f'{left} - {right}'

    def multiplication(self, *operands):
        first, *rest = operands
        for o in rest:
            try:
                first *= o
            except BaseException as e:
                breakpoint()
                raise e

        return first

    def division(self, numerator, denominator):
        return numerator / denominator
        
    def exp(self, left, right):
        # big note here that the units representation in protcur
        # is distinctly NOT algebraic with regard to order of operations
        # this is a structural issue with the parsing output that we
        # correct here -- however it seems likely that at some point
        # someone might actually want to say 10 m ** 6 or something like that?
        if isinstance(left, _Quant):
            left, right = self._merge_dims(left, right)
            return left.magnitude ** right.magnitude * left.units

        try:
            return left ** right
        except BaseException as e:
            breakpoint()
            raise e

    def approximately(self, expression):
        # FIXME TODO I think the parse tree is inverted and approximately
        # goes on the magnitude inside the number instead of wrapping
        # the whole expression
        return Approximately(expression)
        return f'~{expression}'  # FIXME not quite ready

    def plus_or_minus(self, base, error=None):
        if error is None:
            msg = (f'error is a required argument for plus_or_minus '
                   'someone is misusing the notation!')
            e = exc.BadNotationError(msg)
            if self._config.data_failure_mode == mode.FAIL:
                raise e

            logd.error(str(e))
            return e

        if not hasattr(base, 'plus_minus'):  # unconverted
            _base = base
            base = _base * ur.dimensionless #_Quant(base)

        nbase, nerror = self._merge_dims(base, error)
        try:
            return nbase.plus_minus(nerror)  # TODO
        except BaseException as e:
            breakpoint()
            raise e
        #return PlusOrMinus(base, error)

    def _merge_dims(self, *quants):
        """ the sexpr representation only includes the unit once
            since we know we will eventually apply it to all values
            in the quantity, which we do here
            there are probably errors that can creep in because of this
            if the parser spec failes to capture some corner case """

        with_dims = [q for q in quants if hasattr(q, 'dimensionality') and q.dimensionality]

        if with_dims:
            first = with_dims[0]
            for q in with_dims[1:]:
                assert first.check(q.dimensionality)

            fu = first.units
            return [q.to(fu) for q in quants]

        return quants

    def range(self, left, right):
        """ Range faces a similar issue as quantity
            In a purely homogenous system ranges values
            would always have start and stop represented
            in the same units. We can't gurantee that, or
            rather, we could, but the question then becomes
            where to implement the reconciliation between the
            different units, and the answer is 'here' for range
            and for dimensions, this is true even if we are handed
            prefix quantities """
        # FIXME TODO a range of quantities or a quantity of ranges?
        # it is easier to write the range once if the units are
        # homogenous, if they are not then it is a range of quantities
        left, right = self._merge_dims(left, right)

        return self._Range(left, right)

        # TODO abstract this for *quants (i.e. dimensions)
        units = set(e.units for e in (left, right))
        if len(units) == 1:  # otherwise we are going to print both units
            unit = next(iter(units))
            # hack to simplify printing
            if isinstance(unit, self._PrefixUnit):
                left = PrefixQuantity(left.value, unit)
                right = PrefixQuantity(right.value)
            else:
                left = Quantity(left.value)
                right = Quantity(right.value, unit)

        return self._Range(left, right)

    def ratio(self, left, right):
        left, right = self._merge_dims(left, right)
        return self._Ratio(left, right)

    def dimensions(self, *quants):
        # TODO reduce multiple units ?? it 1mm x 1mm x 1mm -> 1x1x1mm
        quants = self._merge_dims(*quants)
        return self._Dimensions(*quants)

    def bool(self, value):
        return boolc(value == '#t')
        #return 'true' if value == '#t' else 'false'

    def _than(self, symbol, left, right):
        s = symbol
        if not right:
            return f'{s} {left}'
        else:
            if len(right) == 1:
                right, = right
                return f'{left} {s} {right}'
            else:
                return f'({s} {left} ' + ' '.join(right) + ')'

    def greater_than(self, left, *right):
        if not right:
            return GreaterThan > left

        return left > right

    def less_than(self, left, *right):
        if not right:
            # interpret as (< quantity left)
            # with an implicit unmeasured quantity
            # basically a combinator
            return LessThan < left

        return left < right

    def greater_than_or_equal(self, left, *right):
        if not right:
            return GreaterThanOrEqual > left

        return left > right

    def less_than_or_equal(self, left, *right):
        if not right:
            # interpret as (< quantity left)
            # with an implicit unmeasured quantity
            # basically a combinator
            return LessThanOrEqual < left

        return left < right


macro = MacroDecorator()
@macro.has_macros
class Protc(ImplFactoryHelper, Interpreter):
    """ definitions for the protc: namespace """

    namespace = 'protc'

    # NOTE namespaces aren't actuall real right now

    _BlackBox = None
    _BlackBoxComponent = None  # dependant continuant?
    _Input = None
    _InputInstance = None
    _Output = None
    _Invariant = None
    _Parameter = None
    _Objective = None
    _Measure = None
    _Aspect = None
    _AspectTerminal = None
    _ExecutorVerb = None
    _Term = None
    _FuzzyQuantity = None

    _SymbolicInput = None
    _SymbolicOutput = None

    _Qualifier = None

    _CircularLink = None

    plus = ParamParser.plus  # FIXME common forms class?

    def black_box(self, black_box_name, *body, prov=None):
        return self._BlackBox(black_box_name, *body, prov=prov)

    def black_box_component(self, black_box_component_name, *body, prov=None):
        return self._BlackBoxComponent(black_box_component_name, *body, prov=prov)

    def input(self, black_box, *body, prov=None):
        bb = self.eval(black_box)
        return self._Input(black_box, *body, prov=prov)

    def input_instance(self, black_box, *body, prov=None):
        if body:
            log.warning('check to see if protc:input-instance '
                        'is supposed to have a body ...\n'
                        f'{body}')
        return self._InputInstance(black_box, *body, prov=prov)

    def output(self, black_box, *body, prov=None):
        return self._Output(black_box, *body, prov=prov)

    def symbolic_input(self, value, *body, prov=None):
        return self._SymbolicInput(value, *body, prov=prov)

    def symbolic_output(self, value, *body, prov=None):
        return self._SymbolicOutput(value, *body, prov=prov)

    def aspect(self, name, *body, prov=None):
        return self._Aspect(name, *body, prov=prov)

    #@macro
    def parameter(self, quantity, *rest, prov=None):  # FIXME and here we see yet another bug in my original implementation
        #""" macro so we can use the parameter parser """
        # no, the namespaceing takes care of it automatically
        #prov_value = self.eval(prov)
        #return self._ParamParser(quantity)
        #*rest, prov = rest_prov
        return self._Parameter(quantity, *rest, prov=prov)#, tuple(rest))

    def invariant(self, quantity, *rest, prov=None):  # FIXME and here we see yet another bug in my original implementation
        #*rest, prov = rest_prov
        if hasattr(quantity, 'prov') and quantity.prov is None and prov is not None:
            quantity.prov = prov  # FIXME fuzzy quantity issue also need to propagate further

        return self._Invariant(quantity, *rest, prov=prov)#, tuple(rest))

    def implied_input(self, args, *body, prov=None):
        return self.input(args, *body, prov=prov)  # FIXME loss of implied

    def implied_output(self, args, *body, prov=None):
        return self.output(args, *body, prov=prov)  # FIXME loss of implied

    def implied_aspect(self, aspect, *body, prov=None):
        return self.aspect(aspect, *body, prov=prov)  # FIXME ... when do we no longer care about impliedness?

    def measure(self, variable_name, *body, prov=None):
        return self._Measure(variable_name, *body, prov=prov)

    def result(self, value, *body, prov=None):  # XXX
        breakpoint()
        return prov

    def repeat(self, value, *body, prov=None):  # XXX
        raise NotImplementedError
        #breakpoint()
        # FIXME TODO WAT
        return

    def process(self, value, *body, prov=None):  # XXX
        raise NotImplementedError
        breakpoint()

    def qualifier(self, value, *body, prov=None):  # XXX
        return self._Qualifier(value, *body, prov=prov)

    def executor_verb(self, verb, *body, prov=None):
        return self._ExecutorVerb(verb, *body, prov=prov)

    def objective(self, value, *body, prov=None):
        return self._Objective(value, *body, prov=prov)

    def term(self, curie, label, original=None):
        return self._Term(curie, label, original)

    ## FIXME unnamespaced cases
    #def approximately(self, quantity):
        # FIXME
        #return Approximately(quantity)
    def fuzzy_quantity(self, fuzzy, aspect_string):  # FIXME another example of a bad impl which can't access the prov
        # also FIXME this needs to be rewritten so that the aspect is on the outside
        # I don't think we can fix this here
        # indeed we can't we have to rewrite starting from the invariant

        if not hasattr(self.__class__, '_fuzzy_warned'):
            log.critical('FIXME reminder to fix fuzzy-quantity issues')
            self.__class__._fuzzy_warned = True

        # TODO fuzzy -> controlled vocabulary
        # XXX NOTE (aspect ...) is NOT the same as the controlled term
        fq = self._FuzzyQuantity(fuzzy, self._AspectTerminal(aspect_string))
        fq.aspect.prov = fq
        return fq

    def circular_link(self, value, cycle):
        # FIXME value seems to always be no-type here which seems to be a mistake?
        return self._CircularLink(cycle)
        #return ('circular-link', value, cycle)

    def cycle(self, *cycle_members):
        return ('cycle', *cycle_members)  # hue heu hue hue


setattr(Protc, 'parameter*', Protc.parameter)
setattr(Protc, 'objective*', Protc.objective)
setattr(Protc, '*measure', Protc.measure)


class IdEqSortHelper:

    def __hash__(self):
        return hash((self._value, self.prov))

    def __eq__(self, other):
        return type(self) == type(other) and self._value == other._value

    def __lt__(self, other):
        if type(self) == type(other):
            # TODO body length?
            try:
                return self._value < other._value
            except TypeError:
                return other._value > self._value
        else:
            return self.__class__.__name__ < other.__class__.__name__

    def __le__(self, other):
        return self < other or self == other

    def __gt__(self, other):
        if type(self) == type(other):
            # TODO body length?
            return self._value > other._value
        else:
            return self.__class__.__name__ > other.__class__.__name__

    def __ge__(self, other):
        return self > other or self == other


class BlackBox(IdEqSortHelper, intf.AJ):

    _type = 'protcur:black-box'

    def __init__(self, name, *body, prov=None):
        self.name = name
        self.prov = prov
        self.body = body

    @property
    def _value(self):
        return self.name


class BlackBoxComponent(IdEqSortHelper, intf.AJ):

    _type = 'protcur:black-box-component'

    def __init__(self, name, *body, prov=None):
        self.name = name
        self.prov = prov
        self.body = body

    @property
    def _value(self):
        return self.name


class Input(IdEqSortHelper, intf.AJ):

    _type = 'protcur:input'

    def __init__(self, black_box, *body, prov=None):
        self.black_box = black_box
        self.prov = prov
        self.body = body

    @property
    def _value(self):
        return self.black_box


class InputInstance(IdEqSortHelper, intf.AJ):

    _type = 'protcur:input-instance'

    #__hash__ = Input.__hash__
    #__eq__ = Input.__eq__
    #__lt__ = Input.__lt__
    #__le__ = Input.__le__
    #__gt__ = Input.__gt__
    #__ge__ = Input.__ge__

    def __init__(self, black_box, *body, prov=None):
        self.black_box = black_box
        self.prov = prov
        self.body = body

    @property
    def _value(self):
        return self.black_box


class Output(IdEqSortHelper, intf.AJ):

    _type = 'protcur:output'

    #__hash__ = Input.__hash__
    #__eq__ = Input.__eq__
    #__lt__ = Input.__lt__
    #__le__ = Input.__le__
    #__gt__ = Input.__gt__
    #__ge__ = Input.__ge__

    def __init__(self, black_box, *body, prov=None):
        self.black_box = black_box
        self.prov = prov
        self.body = body

    @property
    def _value(self):
        return self.black_box


class Invariant(intf.AJ):

    _type = 'protcur:invariant'

    def __init__(self, quantity, *rest, prov=None):
        self.quantity = quantity
        self.prov = prov
        self.rest = rest

    @property
    def _value(self):
        return self.quantity

    def __hash__(self):
        to_hash = self.__class__, self.quantity, self.prov, self.rest
        try:
            return hash(to_hash)
        except Exception as e:
            breakpoint()
            'asdf'

    def __eq__(self, other):
        # XXX for sorting only, remember, these are more the python
        # representation of the ast than of the values themselves
        # so actual comparison should be on the value in the node
        return hash(self) == hash(other)

    def __lt__(self, other):
        return not ((self > other) or self == other)

    def __gt__(self, other):
        if type(self) != type(other):
            return False

        if isinstance(self.quantity, str):
            breakpoint()
        qds = self.quantity.dimensionality
        qdo = other.quantity.dimensionality
        if qds == qdo:
            try:
                return self.quantity > other.quantity
            except ValueError:
                # FIXME ... hack to work around the fact
                # that pint quantities don't know what
                # to do with approximate values, but
                # approximate values do know what to do
                # about pint quantities, the only issue
                # is that approximate values are weird
                # so you can't negate their results or
                # else you will get the wrong answer
                # try that one out on a type system ...
                # a boolean type that cannot be negated
                # within a specific scope
                #breakpoint()
                try:
                    return other.quantity <= self.quantity
                except ValueError:
                    breakpoint()
                    'asdf'
        else:
            return tuple(qds.items()) > tuple((qdo.items()))

    def __ge__(self, other):
        return self > other or self == other


class Parameter(intf.AJ):

    _type = 'protcur:parameter'  # XXX Note that we drop the asterisk for this one, may drop it entirely

    __hash__ = Invariant.__hash__
    __eq__ = Invariant.__eq__
    __lt__ = Invariant.__lt__
    __le__ = Invariant.__le__
    __gt__ = Invariant.__gt__
    __ge__ = Invariant.__ge__

    def __init__(self, quantity, *rest, prov=None):
        # NOTE quantity here implies that it implements
        # the quantity interface (though python doesn't formalize that notion)
        # so things like range also count here
        self.quantity = quantity
        self.prov = prov
        self.rest = rest

    @property
    def _value(self):
        return self.quantity


class Objective(intf.AJ):

    _type = 'protcur:objective*'

    def __init__(self, value, *rest, prov=None):
        self.value = value
        self.prov = prov
        self.rest = rest

    @property
    def _value(self):
        return self.value


class Measure(intf.AJ):

    _type = 'protcur:*measure'

    __hash__ = Invariant.__hash__
    __eq__ = Invariant.__eq__
    __lt__ = Invariant.__lt__
    __gt__ = Invariant.__gt__

    def __init__(self, variable_name, *rest, prov=None):
        # NOTE quantity here implies that it implements
        # the quantity interface (though python doesn't formalize that notion)
        # so things like range also count here
        self.quantity = variable_name  # FIXME
        self.prov = prov
        self.rest = rest

    @property
    def _value(self):
        return self.quantity


class Aspect(intf.AJ):

    _type = 'protcur:aspect'

    _aspect_to_dimension = {
        # FIXME this needs to be defined somewhere more visible
        # unfortunately pint can't alias dimension
        # we probably need a general aspect -> dimension mapping
        # since aspect includes distinctions such as a point in
        # time relative to a fixed reference, vs a duration relative
        # to any reference, way more issues with length where we
        # have countless well defined aspects of things that all
        # share the length dimension and depend on context
        # l w h r d c etc. hypotenuse, opposite, adjascent (fun)
        'duration': '[time]',
    }
    @property
    def dimensionality(self):
        if self.name in self._aspect_to_dimension:
            key = self._aspect_to_dimension[self.name]
        else:
            key = f'[{self.name}]'

        if key not in ur._dimensions:
            raise NotImplementedError(f'unknown dimension {key}')

        dim = ur._dimensions[key]
        if not dim.is_base:
            # useful for things like duration
            return dim.reference

        return pint.util.UnitsContainer({key: 1.0})

    def __init__(self, name, *body, prov=None):
        self.name = name
        self.prov = prov
        self.body = body

    @property
    def _value(self):
        if isinstance(self.name, Term):
            return self.name

        else:
            return self.name

    def __hash__(self):
        return hash((self.name, self.prov))

    def __eq__(self, other):
        return hash(self) == hash(other)

    def __lt__(self, other):
        if type(self) == type(other):
            return (self.name < other.name or self.name == other.name
                    and len(self.body) < len(other.body))
        elif isinstance(other, str):
            return self.name < other
        else:
            return self.__class__.__name__ < other.__class__.__name__

    def __le__(self, other):
        return self < other or self == other

    def __gt__(self, other):
        if type(self) == type(other):
            return (self.name > other.name or self.name == other.name
                    and len(self.body) > len(other.body))
        elif isinstance(other, str):
            return self.name > other
        else:
            return self.__class__.__name__ > other.__class__.__name__

    def __ge__(self, other):
        return self > other or self == other


class AspectTerminal(Aspect):
    """ An aspect used as a terminal the needs to be lifted.
        Primariliy used in FuzzyQuantity. """

    def asJson(self):
        out = {
            '@id': self._value,
            '@type': self._type,
            '@value': self.name,
            'node_type': self.__class__.__name__,  # TODO @type is what?
            # don't need dimensionality, can recover from id
        }

    def __hash__(self):
        return hash(self.name)


class ExecutorVerb(IdEqSortHelper, intf.AJ):

    _type = 'protcur:executor-verb'

    def __init__(self, verb, *body, prov=None):
        self.verb = verb
        self.prov = prov
        self.body = body

    @property
    def _value(self):
        # FIXME this one is going to be complicated
        return self.verb


class SymbolicInput(intf.AJ):

    _type = 'protcur:symbolic-input'

    def __init__(self, value, *body, prov=None):
        self.value = value
        self.body = body
        self.prov = prov

    @property
    def _value(self):
        return self.value


class SymbolicOutput(intf.AJ):

    _type = 'protcur:symbolic-output'

    __init__ = SymbolicInput.__init__

    @property
    def _value(self):
        return self.value


class Qualifier(intf.AJ):

    _type = 'protcur:qualifier'

    __init__ = SymbolicInput.__init__

    @property
    def _value(self):
        return self.value


class Term(intf.AJ):

    _OntTerm = None

    def asJson(self):
        if self._OntTerm is None:
            breakpoint()
        # XXX hits many cases where the curie does not map e.g. asp:
        l = self.label if self.label else None
        term = self._OntTerm(self.curie, label=l)
        out = term.asDict()
        out['original'] = self.original
        return out

    @property
    def _value(self):
        """ on the off chance that __str__ is called """
        ori = f' #:original {self.original}' if self.original else ''
        label = f' {json.dumps(self.label)}' if self.label else ' #f'
        return f'(term {self.curie}{label}{ori})'

    def __init__(self, curie, label, original):
        self.curie = curie
        self.label = label
        self.original = original

    def __hash__(self):  # FIXME probably wrong for evaled nodes ...
        return hash((self.__class__, self.curie))

    def __eq__(self, other):
        # FIXME warn on label mismatch?
        return (self.__class__ == other.__class__ and
                self.curie == other.curie)

    def __lt__(self, other):
        if self.label is None or self.label == False:
            return True

        if type(self) == type(other):
            if other.label is None or other.label == False:
                return False

            return self.label < other.label
        elif isinstance(other, str):
            return self.label < other
        else:
            return self.__class__.__name__ < other.__class__.__name__

    def __gt__(self, other):
        if self.label is None or self.label == False:
            return False

        if type(self) == type(other):
            if other.label is None or other.label == False:
                return True

            return self.label > other.label
        elif isinstance(other, str):
            return self.label > other
        else:
            return self.__class__.__name__ > other.__class__.__name__


class FuzzyDef(intf.AJ):
    def __init__(self, aspect, name, expression):
        self.aspect = aspect
        self.name = name
        self.expression = expression


class FuzzyQuantity(intf.AJ):

    _FuzzyDefs = {  # FIXME this should be coming from protc define-fuzzy-quantity
        ('duration', 'overnight'): '',  # 6 < v < 24
        ('temperature', 'room temperature'): '',  # 20 +/- 5
        ('temperature', 'ice cold'): '',  # -10 < v < 4  # melting ice can get quite cold but also quite warm
        ('immersion-type', 'water'): '',
        ('immersion-type', 'oil'): '',
        ('ammount', 'several thousand'): '',  # 1000 < v < 9000
    }

    @property
    def dimensionality(self):
        return self.aspect.dimensionality

    def asJson(self):
        return {
            '@id': 'fuzzy:' + self.fuzzy.replace(' ', '-'),
            '@type': ['owl:Class'],  # class is correct at protocol level
            'aspect': self.aspect.asJson(),
        }

    def __init__(self, fuzzy, aspect):
        self.fuzzy = fuzzy
        self.aspect = aspect

    def __hash__(self):
        return hash((self.__class__,
                        self.fuzzy,
                        self.aspect,))

    def __eq__(self, other):
        # XXX for sorting only, remember, these are more the python
        # representation of the ast than of the values themselves
        # so actual comparison should be on the value in the node
        return hash(self) == hash(other)

    def __ne__(self, other):
        return not self == other

    def __gt__(self, other):  # TODO fuzzy defs
        return False

    def __ge__(self, other):
        return self == other

    def __lt__(self, other):  # TODO fuzzy defs
        return False

    def __le__(self, other):
        return self == other


class CircularLink(intf.AJ):

    def asJson(self):
        return tuple()  # FIXME TODO

    def __init__(self, cycle):
        self.cycle = cycle


Protc.bindImpl(None,
               BlackBox,
               BlackBoxComponent,
               Input,
               InputInstance,
               Output,
               Invariant,
               Parameter,
               Objective,
               Measure,
               Aspect,
               AspectTerminal,
               ExecutorVerb,
               SymbolicInput,
               SymbolicOutput,
               Qualifier,
               Term,
               FuzzyQuantity,
               CircularLink,
               )

class RacketParser(ImplFactoryHelper, SExpr):  # XXX TODO

    _Protc = None

    ParseFailure = exc.ParseFailure

    def __new__(cls, string_to_parse, sexp=None, rest_ok=True):
        if sexp is None:  # needed for copy to work happily
            success, sexp, rest = racket.exp(string_to_parse)
            if rest and not rest_ok:
                raise ValueError(f'Failed to parse suffix {rest}')

        if sexp is None:
            raise exc.ParseFailure(string_to_parse)

        if isinstance(sexp, tuple):
            self = super().__new__(cls, sexp)
            self._input = string_to_parse
        elif isinstance(sexp, str):
            self = RacketString(sexp)
            self._input = string_to_parse
        elif isinstance(sexp, Number):
            self = RacketNumber(sexp)
            self._input = string_to_parse
        else:
            raise NotImplementedError(f"Not converting {type(sexp)} yet. Or you have a bug.")

        return self

    def __call__(self, string_to_parse):
        # FIXME, do we want this version ??
        pass

    def asPython(self):
        return self._Protc()(self)  # FIXME ... needs to be more flexible

    def __getnewargs_ex__(self):
        return (self._input,), {}

    def __copy__(self):
        cls = self.__class__
        result = cls.__new__(cls, self._input, sexp=self)
        result.__dict__.update(self.__dict__)
        return result

    def __deepcopy__(self, memo):
        cls = self.__class__
        result = cls.__new__(cls, self._input, sexp=self)
        memo[id(self)] = result
        for k, v in self.__dict__.items():
            setattr(result, k, copy.deepcopy(v, memo))
        return result


RacketParser.bindImpl(None, Protc)


macro = MacroDecorator()
@macro.has_macros
class Hyp(ImplFactoryHelper, Interpreter):
    """ definitions for the hyp: namespace """

    namespace = 'hyp'

    _HypothesisAnno = staticmethod(lambda a: a)  # hack for simple defaults

    def hyp(self, id):
        return self._HypothesisAnno(id)


setattr(Hyp, '', Hyp.hyp)  # I knew it was coming, and the fact that it works is kind of amusing


# default configuration interpreters
# override these after import if there are custom formats that you want export to
UnitsHelper.setup()  # FIXME XXX this is really expensive to call ...
ParamParser.bindImpl(None,
                     Unit,
                     PrefixUnit,
                     #Quantity,
                     #PrefixQuantity,
                     Range,
                     Ratio,
                     Dimensions)

# the parsing api for external consumption

class UnitsParser(UnitsHelper, ImplFactoryHelper, SExpr):  # FIXME this needs to be extnesible as well

    _ParamParser = None

    ParseFailure = exc.ParseFailure

    def __new__(cls, string_to_parse, sexp=None, rest_ok=True):
        if sexp is None:  # needed for copy to work happily
            success, sexp, rest = cls._parameter_expression(string_to_parse)
            if rest and not rest_ok:
                # TODO try to failover to the pint parser for coverage
                raise ValueError(f'Failed to parse suffix {rest}')

        self = super().__new__(cls, sexp)
        self._input = string_to_parse
        return self

    def __call__(self, string_to_parse):
        # FIXME, do we want this version ??
        pass

    def asPython(self):
        return self._ParamParser()(self)  # FIXME ... needs to be more flexible

    def __getnewargs_ex__(self):
        return (self._input,), {}

    def __copy__(self):
        cls = self.__class__
        result = cls.__new__(cls, self._input, sexp=self)
        result.__dict__.update(self.__dict__)
        return result

    def __deepcopy__(self, memo):
        cls = self.__class__
        result = cls.__new__(cls, self._input, sexp=self)
        memo[id(self)] = result
        for k, v in self.__dict__.items():
            setattr(result, k, copy.deepcopy(v, memo))
        return result


UnitsParser.bindImpl(None, ParamParser)


# pretty printing config
def _pprint_operation(self, object, stream, indent, allowance, context, level):
    #value = object.format_value(indent)  # how the heck does this work?
    value = object.__repr__(indent)  # how the heck does this work?
    stream.write(value)


pprint.PrettyPrinter._dispatch[SExpr.__repr__] = _pprint_operation

Interpreter._namespace_classes = {c.namespace:c for c in Interpreter.__subclasses__()
                                  if hasattr(c, 'namespace')}
