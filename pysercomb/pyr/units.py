""" Python class representation for the output of the units parser. """
import pprint
import itertools
from enum import Enum
from pathlib import Path
from pysercomb.parsers.units import get_unit_dicts, _plus_or_minus, make_unit_parser
from protcur.config import __script_folder__ as pasf


def chain(*tups):
    for t in tups:
        yield from reversed(t)  # reversed because the piority ordering is inverted


class UnitsHelper:

    @classmethod
    def setup(cls):
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

        # don't actually need this because its in the ast
        #prefix_units = set(unit for abbrev, unit in
                        #chain(units_extra_prefix,
                                #units_dimensionless_prefix))

        cls.prefix_dict = {prefix:abbrev for abbrev, prefix in prefixes_si}


class ProtcParameter:
    format_nl =  '*', '/', 'range', 'plus-or-minus', 'param:dimensions'
    format_nl_long =  '^'

    def __init__(self, tuple_repr):
        self._tuple = tuple_repr
        self._ir = ParamParser(self._tuple)

    def isLongNL(self, tuple_):
        if tuple_[0] in self.format_nl_long:
            t1 = type(tuple_[1]) is tuple
            t2 = type(tuple_[2]) is tuple
            if t1 and t2:
                if len(tuple_[1]) > 2 or len(tuple_[2]) > 2:
                    return True
            elif t1 and len(tuple_[1]) > 3:
                return true
            elif t2 and len(tuple_[2]) > 3:
                return true
        return False

    def format_value(self, localIndent=0, depth=0, tuple_=None):#, LID=''):
        if tuple_ is None:
            tuple_ = self._tuple
            #from IPython import embed
            #import sys
            #embed()
            #sys.exit(0)

        out = ''
        if tuple_:
            newline = tuple_[0] in self.format_nl or self.isLongNL(tuple_)
            indent_for_this_loop = localIndent + len('(') + len(tuple_[0]) + len(' ')  # vim fail )
            indent_for_next_level = indent_for_this_loop
            #indent_this = LID + '(' + tuple_[0] + ' '  # vim fail )
            #indent_next = indent_this
            for i, v in enumerate(tuple_):
                if newline and i > 1:
                    out += '\n' + ' ' * indent_for_this_loop
                    #out += '\n' + indent_this
                if type(v) is tuple:
                    v = self.format_value(indent_for_next_level, depth + 1, v)#, LID=indent_next)
                if v is not None:
                    v = str(v)
                    if out and out[-1] != ' ':
                        out += ' ' + v
                        if i > 1 or not newline:
                            indent_for_next_level += len(' ') + len(v) # unlike at the top v already has ( prepended if it exists
                            #indent_next += ' ' + v
                    else:  # we are adding indents
                        out += v
        if out:
            return '(' + out + ')'

    def _repr_pretty_(self, p, cycle):
        if cycle:
            p.text(self.__class__.__name__ + '(WAT...)')
        else:
            indent = p.output_width
            p.text(self.__repr__(indent))

    def __repr__(self, indent=0):
        cname = self.__class__.__name__
        _tuple = self.format_value(len(cname) + 1 + indent) 
        return cname + f'({_tuple})'

    @property
    def for_triples(self):
        return self._ir.triples_conbinator

    @property
    def for_text(self):
        return str(self._ir)

    def asPython(self):
        return self._ir


class ProtcParameterParser(UnitsHelper, ProtcParameter):

    def __init__(self, value):
        success, ast, rest = self._parameter_expression(value)
        super().__init__(ast)


class LoR:
    op = None
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __str__(self):
        return f'{self.left!r}{self.op}{self.right!r}'

    def __repr__(self):
        return f'{self.__class__.__name__}({self.left!r}, {self.right!r})'


class Unit:
    def __init__(self, unit, prefix=None):
        self.unit = unit
        self.prefix = prefix

    def __repr__(self):
        prefix = self.prefix if self.prefix else ''
        return f'{prefix}{self.unit}'

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


class Expr:
    op = None

    def __add__(self, other):
        return Add(self, other)

    def __iadd__(self, other):
        return self.__add__(other)

    def __mul__(self, other):
        return Mul(self, other)

    def __imul__(self, other):
        return self.__mul__(other)

    def __truediv__(self, other):
        return Div(self, other)

    def __pow__(self, other):
        return Exp(self, other)

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


class Quantity(Expr):

    def __init__(self, value, unit):
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
        return f'{self.__class__.__name__}({self.value!r}, {self.unit!r})'

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
    op = '-'
    def __init__(self, start, stop, unit=None):
        self.start = start.value
        self.stop = stop.value
        # FIXME match units
        self.unit = (start.unit
                     if start.unit is not None
                     else (stop.unit if stop.unit is not None
                           else unit))

    def __str__(self):
        unit = self.unit if self.unit else ''
        return f'{self.start}{self.op}{self.stop}{unit}'

    def __repr__(self):
        unit = f', {self.unit!r}' if self.unit else ''
        return f'{self.__class__.__name__}({self.start!r}, {self.stop!r}{unit})'


class PlusOrMinus(Range):
    op = _plus_or_minus


class Dilution(LoR):
    op = ':'


class Dimensions:
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
        return self.op.join(self.values) + (str(self.unit) if self.unit else '')


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
        failure_mode = mode.WARN
        
    _renames = {
        '+': 'plus',
        '-': 'minus',
        '*': 'multiplication',  # FIXME unit vs normal implies macro
        '/': 'division',
        '^': 'exp',
        '<': 'less_than',
        '>': 'greater_than',
    }

    def __init__(self, sexp, environment=None):
        self.sexp = sexp
        self.environment = environment

    def lisp_to_python(self, lisp_identifier):
        if lisp_identifier in self._renames:
            return self._renames[lisp_identifier]

        return lisp_identifier.split(':', 1)[-1].replace('-', '_')

    def __call__(self):
        return self.eval(self.sexp)


    def eval(self, expression):
        if isinstance(expression, tuple) or isinstance(expression, list):
            # tuple unpacking produces lists because generators
            # have unkown lenght
            tup = expression
            if not tup:
                return None
        else:
            return str(expression)

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
class ParamParser(UnitsHelper, Interpreter):
    """ definitions for the param: namespace """

    def parse_failure(self, *args):
        return None

    def expr(self, sexp):
        return sexp

    def unit_expr(self, expression):  # TODO probably a macro
        #if ' * ' in expression:
            # FIXME ... would prefer to detect ahead of time
            #expression = expression.replace(' * ', '*')

        return expression

    @macro  # oops, sometime unit expressions are hidden in units :x
    def unit(self, unit, prefix=None):
        if unit[0] == '/':  # FIXME broken parser output
            return self.eval(unit)

        unit = self.eval(unit)
        if prefix is not None:
            prefix = self.eval(prefix)
        full = ('weeks', 'days', 'months', 'years')
        unquoted_unit = unit[1:]
        if prefix is None and unquoted_unit in full:
            return Unit(unquoted_unit)

        p = self._prefix(prefix)
        u = self._unit(unquoted_unit)

        return Unit(u, p)

    def _prefix(self, prefix):
        if prefix:
            prefix = prefix[1:]
            return self.prefix_dict[prefix]
        else:
            return None

    def _unit(self, unquoted_unit):
        return self.unit_dict[unquoted_unit]

    def prefix_unit(self, unit):
        return self.unit(unit)

    #@macro(True, False)  # TODO specify which values can be evaluated
    @macro
    def quantity(self, value, unit):
        """ FIXME this prefix_unit issue reveals that this should
            really be prefix-quantity so that it doesn't have to
            be a macro that looks for a prefix-unit """
        value = self.eval(value)
        unit_value = self.eval(unit)
        if unit and unit[0] == 'param:prefix-unit':
            return PrefixQuantity(value, unit_value)
        else:
            return Quantity(value, unit_value)

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
            if self._config.failure_mode == mode.FAIL:
                raise TypeError(f'error is a required argument for plus_or_minus '
                                'someone is misusing the notation!')

            return 

        return PlusOrMinus(base, error)

    def range(self, start, stop):
        return Range(start, stop)

    def dilution(self, left, right):
        return Dilution(left, right)

    def dimensions(self, *quants):
        # TODO reduce multiple units ?? it 1mm x 1mm x 1mm -> 1x1x1mm
        return Dimensions(*quants)

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


UnitsHelper.setup()


def _pprint_operation(self, object, stream, indent, allowance, context, level):
    #value = object.format_value(indent)  # how the heck does this work?
    value = object.__repr__(indent)  # how the heck does this work?
    stream.write(value)


pprint.PrettyPrinter._dispatch[ProtcParameter.__repr__] = _pprint_operation
