""" Python class representation for the output of the units parser. """
import pprint

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
    def for_text(self):
        return TextPP(self._tuple)()


class ProtcParameterParser(UnitsHelper, ProtcParameter):

    def __init__(self, value):
        success, ast, rest = self._parameter_expression(value)
        super().__init__(ast)


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
class TextPP(UnitsHelper):
    _renames = {
        '+': 'plus',
        '-': 'minus',
        '*': 'multiplication',  # FIXME unit vs normal implies macro
        '/': 'division',
        '^': 'exp',
        '<': 'less_than',
        '>': 'greater_than',
    }
    def __init__(self, tup):
        self.tup = tup

    def name_to_python(self, first):
        if first in self._renames:
            return self._renames[first]

        return first.split(':', 1)[-1].replace('-', '_')

    def __call__(self):
        return self.eval(self.tup)

    def eval(self, thing):
        if isinstance(thing, tuple) or isinstance(thing, list):
            # tuple unpacking produces lists because generators
            # have unkown lenght
            tup = thing
            if not tup:
                return ''  # ah nil
        else:
            return str(thing)

        first, *rest = tup
        pyfirst = self.name_to_python(first)
        function_or_macro = getattr(self, pyfirst)
        if pyfirst in self._macros:
            return function_or_macro(*rest)

        if isinstance(rest, list) or isinstance(rest, tuple):
            value = [self.eval(r) for r in rest]

        return function_or_macro(*value)  # apply is * woo

    def expr(self, tup):
        return tup
        #return self.eval(tup)

    def range(self, start, stop):
        return f'{start}-{stop}'

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
            return ' ' + unquoted_unit

        p = self._prefix(prefix)
        u = self._unit(unquoted_unit)

        return f'{p}{u}'

    def _prefix(self, prefix):
        if prefix:
            prefix = prefix[1:]
            return self.prefix_dict[prefix]
        else:
            return ''

    def _unit(self, unquoted_unit):
        return self.unit_dict[unquoted_unit]

    def prefix_unit(self, unit):
        return self.unit(unit)

    def unit_expr(self, expression):  # TODO probably a macro
        if ' * ' in expression:
            # FIXME ... would prefer to detect ahead of time
            expression = expression.replace(' * ', '*')

        return expression

    #@macro(True, False)  # TODO specify which values can be evaluated
    @macro
    def quantity(self, value, unit):
        """ FIXME this prefix_unit issue reveals that this should
            really be prefix-quantity so that it doesn't have to
            be a macro that looks for a prefix-unit """
        value = self.eval(value)
        unit_value = self.eval(unit)
        if unit and unit[0] == 'param:prefix-unit':
            return unit_value + value
        else:
            return value + unit_value

    def parse_failure(self, *args):
        return ''  # TODO

    def plus(self, *operands):
        sep = ' + '
        return sep.join(operands)

    def minus(self, left, right):
        return f'{left} - {right}'

    def multiplication(self, *operands):
        sep = ' * '  # '*' for unit?
        return sep.join(operands)

    def division(self, numerator, denominator):
        if denominator.startswith(' ') and denominator.endswith('s'):
            # days hours etc
            denominator = denominator[1:-1]

        return f'{numerator}/{denominator}'
        
    def exp(self, left, right):
        return f'{left}^{right}'

    def approximately(self, expression):
        return f'~{expression}'

    def plus_or_minus(self, base, error):
        return f'{base}{_plus_or_minus}{error}'

    def dilution(self, left, right):
        return f'{left}:{right}'

    def dimensions(self, *quants):
        # TODO reduce multiple units ?? it 1mm x 1mm x 1mm -> 1x1x1mm
        return ' x '.join(quants)

    def bool(self, value):
        return 'true' if value == '#t' else 'false'

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
        self._than('>', left, right)

    def less_than(self, left, *right):
        self._than('<', left, right)


UnitsHelper.setup()


def _pprint_operation(self, object, stream, indent, allowance, context, level):
    #value = object.format_value(indent)  # how the heck does this work?
    value = object.__repr__(indent)  # how the heck does this work?
    stream.write(value)


pprint.PrettyPrinter._dispatch[ProtcParameter.__repr__] = _pprint_operation
