""" Python class representation for the output of the units parser. """
import pprint
import itertools
from enum import Enum
from pathlib import Path
from pysercomb import exceptions as exc
from pysercomb.utils import log, logd
from pysercomb.parsers.units import get_unit_dicts, _plus_or_minus, make_unit_parser
from protcur.config import __script_folder__ as pasf


def chain(*tups):
    for t in tups:
        yield from reversed(t)  # reversed because the piority ordering is inverted


class UnitsHelper:

    @classmethod
    def setup(cls):
        """ call setup on UnitsHelper once, not on subclasses """

        units_path = Path(pasf, '../../protc-lib/protc/units')
        dicts = get_unit_dicts(units_path)

        (parameter_expression, quantity, unit, *_,
         debug_dict) = make_unit_parser(dicts=dicts)

        cls._parameter_expression = staticmethod(parameter_expression)

        gs = globals()
        for dict_ in dicts:
            gs.update(dict_)

        cls.unit_dict = {unit:abbrev for abbrev, unit in
                          chain(units_si,
                                units_extra,
                                units_extra_prefix,
                                units_dimensionless,
                                units_dimensionless_prefix,
                                units_imp,)}

        cls.unit_dict_inv = {a:u for u, a in cls.unit_dict.items()}

        # don't actually need this because its in the ast
        #prefix_units = set(unit for abbrev, unit in
                        #chain(units_extra_prefix,
                                #units_dimensionless_prefix))

        cls.prefix_dict = {prefix:abbrev for abbrev, prefix in prefixes_si}


class SExpr(tuple):
    """ This class can act as a pretty formatter for s-exprs or it
        can act as a pretty printing object. To use it as a formatter
        initialize it without any arguments as spp = SexpPrettyPrinter()
    """

    format_nl =  '*', '/', 'range', 'plus-or-minus', 'param:dimensions'
    format_nl_long =  '^',  # the empty string is in all strings, so have to use tuple

    def __call__(self, sexp, indent=0):
        return self.__repr__(indent, sexp)

    def isLongNL(self, sexp):
        # and right here is where you want let
        # you could skip this branch entirely except that
        # it is much harder to read when you can't assign
        # t1 and t2 but you have to assign them after the branch
        if sexp[0] not in self.format_nl_long:
            return

        _op, s1, s2 = sexp
        t1 = type(s1) is tuple
        t2 = type(s2) is tuple
        # have to defer the call to len until after the type check
        return (t1 and t2 and (len(s1) > 2 or len(s2) > 2) or
                t1 and len(s1) > 3 or
                t2 and len(s2) > 3)

        if sexp[0] in self.format_nl_long:
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

    def format_value(self, sexp, localIndent=0, depth=0):
        out = ''
        if sexp:
            newline = sexp[0] in self.format_nl or self.isLongNL(sexp)
            indent_for_this_loop = localIndent + len('(') + len(sexp[0]) + len(' ')  # vim fail )
            indent_for_next_level = indent_for_this_loop
            for i, element in enumerate(sexp):
                if newline and i > 1:
                    out += '\n' + ' ' * indent_for_this_loop

                if isinstance(element, tuple):
                    element = self.format_value(element, indent_for_next_level, depth + 1)

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


class LoR:
    op = None
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __str__(self):
        return f'{self.left}{self.op}{self.right}'

    def __repr__(self):
        return f'{self.__class__.__name__}({self.left!r}, {self.right!r})'


class UnitSuffix(UnitsHelper, str):
    @property
    def fullName(self):
        try:
            return self.unit_dict_inv[self]
        except KeyError:
            # FIXME we need an internal representation for composed units
            # but not today!
            return None


class Unit:

    full = ('weeks', 'days', 'months', 'years')

    def __init__(self, unit, prefix=None):
        self.unit = UnitSuffix(unit)
        self.prefix = prefix

    def __str__(self):
        prefix = self.prefix if self.prefix else ''
        unit = (' ' + self.unit.fullName
                if self.unit.fullName in self.full
                else self.unit)

        return f'{prefix}{unit}'

    def __repr__(self):
        return repr(str(self))

    def __mul__(self, other):
        return self.__class__(f'{self}*{other}')

    def __imul__(self, other):
        return self.__mul__(other)

    def __rtruediv__(self, other):
        return self.__class__(f'{self}/{other}')

    def __pow__(self, other):
        return self.__class__(f'{self}^{other}')

    def __truediv__(self, other):
        # TODO do the unit math ...
        return self.__class__(f'{self}/{other}')

    def __eq__(self, other):
        # TODO do the unit math
        return self.unit == other.unit and self.prefix == other.prefix

    def __hash__(self):
        return hash((hash(self.__class__), self.unit, self.prefix))


class PrefixUnit(Unit):
    """ There are some use cases for the unit also carrying this information """

    def __init__(self, unit, prefix=None):
        if isinstance(unit, Unit):
            super().__init__(unit.unit, unit.prefix)
        else:
            super().__init__(unit, prefix)


class Expr:
    op = None

    def __add__(self, other):
        return self._Add(self, other)

    def __iadd__(self, other):
        return self.__add__(other)

    def __mul__(self, other):
        return self._Mul(self, other)

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


class Add(LoR, Expr):
    op = '+'


class Mul(LoR, Expr):
    op = '*'


class Div(LoR, Expr):
    op = '/'


class Exp(LoR, Expr):
    op = '^'


Expr._Add = Add
Expr._Mul = Mul
Expr._Div = Div
Expr._Exp = Exp


class Quantity(Expr):

    def __init__(self, value, unit=None):
        self.value = value
        self.unit = unit

    def __str__(self):
        unit = self.unit if self.unit else ''
        return f'{self.value}{unit}'

    def __repr__(self):
        unit = f', {self.unit!r}' if self.unit else ''
        return f'{self.__class__.__name__}({self.value!r}{unit})'


class PrefixQuantity(Quantity):
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


class Range(Expr):
    """ This is a non-homogenous range, units may differ """
    op = '-'
    def __init__(self, start, stop):
        self.start = start
        self.stop = stop
        # unit representation is dealt with by the parser
        # range could figure it out now with the info
        # provided, but for now is just going to be a dumb
        # container

    def __str__(self):
        return f'{self.start}{self.op}{self.stop}'

    def __repr__(self):
        return f'{self.__class__.__name__}({self.start!r}, {self.stop!r})'


class PlusOrMinus(Range):
    op = _plus_or_minus


class Dilution(Expr, LoR):
    op = ':'


class Dimensions(Expr):
    op = 'x'
    def __init__(self, *dims, unit=None):
        unit = set(d.unit for d in itertools.chain(dims, (unit,)) if d and d.unit is not None)
        if len(unit) > 1:
            raise ValueError(f'More than one unit! {unit}')
        elif unit:
            self.unit = next(iter(unit))
        else:
            self.unit = None

        self.values = tuple(d.value for d in dims)

    def __str__(self):
        return self.op.join(str(v) for v in self.values) + (str(self.unit) if self.unit else '')


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

    def __init__(self, environment=None):
        self.environment = environment
        self.pretty_printer = SExpr()

    def lisp_to_python(self, lisp_identifier):
        if lisp_identifier in self._renames:
            return self._renames[lisp_identifier]

        return lisp_identifier.split(':', 1)[-1].replace('-', '_')

    def __call__(self, sexp):
        return self.eval(sexp)

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
        pyfirst = self.lisp_to_python(first)
        function_or_macro = getattr(self, pyfirst)
        if pyfirst in self._macros:
            return function_or_macro(*rest)

        if isinstance(rest, list) or isinstance(rest, tuple):
            value = [self.eval(r) for r in rest]

        return function_or_macro(*value)  # apply is * woo


macro = MacroDecorator()
@macro.has_macros
class _ParamParser(UnitsHelper, Interpreter):
    """ definitions for the param: namespace """

    _Unit = None
    _PrefixUnit = None
    _Quantity = None
    _PrefixQuantity = None
    _Range = None
    _Dilution = None
    _Dimensions = None

    @classmethod
    def bindPython(cls, *classes):
        """ returns a new interpreter bound to a particular implementation of
            Unit

            specify the underlying python classes that the parser targets
            this is sort of like being able to change the #lang you are using """
        class_dict = {'_' + cls.__name__:cls for cls in classes}
        return type('ParamParser', (cls,), class_dict)

    def parse_failure(self, *args):
        e = exc.ParseFailure('no context to know what the expr was')
        if self._config.parser_failure_mode == mode.FAIL:
            raise e

        logd.error(str(e))
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

        unquoted_unit = quoted_unit[1:]
        p = self._prefix(prefix)
        u = self._unit(unquoted_unit)

        return self._Unit(u, p)

    def _prefix(self, prefix):
        if prefix:
            prefix = prefix[1:]
            return self.prefix_dict[prefix]
        else:
            return None

    def _unit(self, unquoted_unit):
        return self.unit_dict[unquoted_unit]

    def prefix_unit(self, unit):
        return self._PrefixUnit(self.unit(unit))

    #@macro(True, False)  # TODO specify which values can be evaluated
    @macro
    def quantity(self, value, unit):
        """ FIXME this prefix_unit issue reveals that this should
            really be prefix-quantity so that it doesn't have to
            be a macro that looks for a prefix-unit """

        value = self.eval(value)
        unit_value = self.eval(unit)
        if unit and unit[0] == 'param:prefix-unit':
            return self._PrefixQuantity(value, unit_value)
        else:
            return self._Quantity(value, unit_value)

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
            first *= o

        return first

    def division(self, numerator, denominator):
        return numerator / denominator
        
    def exp(self, left, right):
        return left ** right

    def approximately(self, expression):
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

        return PlusOrMinus(base, error)

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
        # TODO abstract this for *quants (i.e. dimensions)
        units = set(e.unit for e in (left, right))
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
        return self._Dilution(left, right)

    def dimensions(self, *quants):
        # TODO reduce multiple units ?? it 1mm x 1mm x 1mm -> 1x1x1mm
        return self._Dimensions(*quants)

    def bool(self, value):
        return value == '#t'
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
class Protc(Interpreter):
    """ definitions for the protc: namespace """

    def black_box(self, args):
        pass
    def black_box_component(self, args):
        pass
    def input(self, args):
        pass
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


# the parsing api for external consumption

class UnitsParser(UnitsHelper, SExpr):

    def __new__(cls, string_to_parse):
        success, ast, rest = cls._parameter_expression(string_to_parse)
        self = super().__new__(cls, ast)
        return self


# pretty printing config
def _pprint_operation(self, object, stream, indent, allowance, context, level):
    #value = object.format_value(indent)  # how the heck does this work?
    value = object.__repr__(indent)  # how the heck does this work?
    stream.write(value)


pprint.PrettyPrinter._dispatch[SExpr.__repr__] = _pprint_operation


# default configuration interpreters
# override these after import if there are custom formats that you want export to
UnitsHelper.setup()
ParamParser = _ParamParser.bindPython(Unit,
                                      PrefixUnit,
                                      Quantity,
                                      PrefixQuantity,
                                      Range,
                                      Dilution,
                                      Dimensions)
