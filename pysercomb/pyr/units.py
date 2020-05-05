""" Python class representation for the output of the units parser.
    You must call UnitHelper.setup() in the client code before you can use this. """
import copy
import pprint
import itertools
from enum import Enum
from urllib.parse import quote
from pathlib import Path
import pint
from pysercomb import exceptions as exc
from pysercomb.utils import log, logd, express
from pysercomb.types import TypeCaster, boolc, strc
from pysercomb.parsers.units import _plus_or_minus
from .core import ImplFactoryHelper , UnitsHelper, Expr
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
ur.load_definitions((Path(__file__).parent / 'pyr_units.txt').as_posix())
ur.default_system = 'mgs'  # SNAAAKKEEEEE system


class _Unit(intf.Unit, ur.Unit):
    def format_babel(self, spec='', **kwspec):
        spec = spec or self.default_format

        if '~' in spec:
            #if self.dimensionless:
                #return ''
            units = pint.unit.UnitsContainer(dict((self._REGISTRY._get_symbol(key),
                                                   value)
                                                  for key, value in self._units.items()))
            spec = spec.replace('~', '')
        else:
            units = self._units

        return '%s' % (units.format_babel(spec, **kwspec))

    def json(self):
        return str(self)

    def asRdf(self):
        return unit[quote(str(self), safe=tuple())]

    def __reduce__(self):
        f, (unit, container) = super().__reduce__()
        return f, (ur.Unit, container)


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
        return dict(type=self.tag, value=self.magnitude, unit=self.units.json())

    @classmethod
    def fromJson(cls, json):
        assert json['type'] == cls.tag
        return cls(json['value'], json['unit'])

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

    @property
    def ttl(self):
        graph = rdflib.Graph()
        OntCuries.populate(graph)
        [graph.add(t) for t in self.asRdf(rdflib.BNode())]
        return graph.serialize(format='nifttl')

    def __reduce__(self):
        f, (quant, mag, *units) = super().__reduce__()
        return f, (ur.Quantity, mag, *units)


ur.Unit = _Unit
ur.Quantity = _Quant


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
    def format_value(cls, sexp, localIndent=0, depth=0):
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
                    out += '\n' + ' ' * indent_for_this_loop

                if isinstance(element, tuple):
                    element = cls.format_value(element, indent_for_next_level, depth + 1)

                if element is not None:
                    strelement = str(element)
                    if out and out[-1] != ' ':
                        out += ' ' + strelement
                        if i > 1 or not newline:
                            # unlike at the top strelement already has ( prepended if it exists
                            indent_for_next_level += len(' ') + len(strelement)

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


class _Than:
    op = None
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


class GreaterThan(_Than, metaclass=gtclass):
    op = '>'


class Range(intf.Range, Oper):
    """ This is a non-homogenous range, units may differ """
    op = '-'

    def __init__(self, left, right):
        self.left = left
        self.right = right
        # unit representation is dealt with by the parser
        # range could figure it out now with the info
        # provided, but for now is just going to be a dumb
        # container

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and
                self.left == other.left and
                self.right == other.right)

    def __mul__(self, other):
        # FIXME rmul on units?
        if isinstance(other, _Unit):
            return self.__class__(self.left * other, self.right * other)
        else:
            raise NotImplementedError

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


class Dilution(LoR):
    op = ':'


class Dimensions(Oper):
    op = 'x'
    def __init__(self, *quants):
        self.quants = quants

    def __str__(self):
        return f' {self.op} '.join(v.format_babel('~', locale='en_US') for v in self.quants)

    def __repr__(self):
        return str(self)

    def asRdf(self):
        self.dims


class Approximately(Oper):
    """ No numercial uncertainty give, but with facilities to operationalize it. """

    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return f'~{self.expr}'

    def __repr__(self):
        return f'{self.__class__.__name__}({self.expr!r})'

    def quantify(self, error, relative=False):
        """ provide concrete values for the approximateness of approximately """
        return self.expr.plus_or_minus(error, relative)


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
    }

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
        try:
            python_repr = self.eval(sexp)
        except exc.ParseFailure:
            # FIXME this wrapping the top level in an exception handler ... tisk tisk (hah)
            raise exc.ParseFailure(sexp._input)

        python_repr._sexp = sexp
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
            return expression

        first, *rest = tup
        namespace, pyfirst = self.lisp_to_python(first)
        function_or_macro = getattr(namespace, pyfirst)
        if pyfirst in self._macros:
            return function_or_macro(*rest)

        if isinstance(rest, list) or isinstance(rest, tuple):
            value = [self.eval(r) for r in rest]

        return function_or_macro(*value)  # apply is * woo

    def quote(self, expression):
        return expression


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

    def parse_failure(self, *args):
        e = exc.ParseFailure
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
    def quantity(self, value, unit):
        """ FIXME this prefix_unit issue reveals that this should
            really be prefix-quantity so that it doesn't have to
            be a macro that looks for a prefix-unit """

        value = self.eval(value)
        value = value if value else 1  # multiplication by 1 for units if the unit is None we get zero?
        unit_value = self.eval(unit)
        # the null unit is just null so we have to handle that since it has no type
        if unit_value is None:
            unit_value = ur.dimensionless

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
            return [fu * q.magnitude for q in quants]
        
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

    def dilution(self, left, right):
        left, right = self._merge_dims(left, right)
        return self._Dilution(left, right)

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


macro = MacroDecorator()
@macro.has_macros
class Protc(ImplFactoryHelper, Interpreter):
    """ definitions for the protc: namespace """

    namespace = 'protc'

    _BlackBox = None
    _Input = None
    _InputInstance = None
    _Invariant = None
    _Parameter = None
    _Aspect = None

    # NOTE namespaces aren't actuall real right now

    def black_box(self, args):
        pass
    def black_box_component(self, args):
        pass
    def input(self, black_box, prov, *rest):
        bb = self.eval(black_box)
        return self._Input(black_box, prov)

    def output(self, args):
        pass
    def aspect(self, args):
        pass
    def parameter(self, args):
        pass
    def invariant(self, args):
        pass
    def implied_input(self, args):
        pass
    def implied_output(self, args):
        pass
    def implied_aspect(self, args):
        pass
    def measure(self, args):
        pass
    def result(self, args):
        pass

    def term(self, args):
        pass


# default configuration interpreters
# override these after import if there are custom formats that you want export to
UnitsHelper.setup()
ParamParser.bindImpl(None,
                     Unit,
                     PrefixUnit,
                     #Quantity,
                     #PrefixQuantity,
                     Range,
                     Dilution,
                     Dimensions)

# the parsing api for external consumption

class UnitsParser(UnitsHelper, ImplFactoryHelper, SExpr):  # FIXME this needs to be extnesible as well

    _ParamParser = None

    ParseFailure = exc.ParseFailure

    def __new__(cls, string_to_parse, sexp=None, rest_ok=True):
        if sexp is None:  # needed for copy to work happily
            success, sexp, rest = cls._parameter_expression(string_to_parse)
            if rest and not rest_ok:
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
