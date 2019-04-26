""" Python class representation for the output of the units parser. """
import pprint

from pathlib import Path
from pysercomb.parsers.units import get_unit_dicts
from protcur.config import __script_folder__ as pasf


def chain(*tups):
    for t in tups:
        yield from t

units_path = Path(pasf, '../../protc-lib/protc/units')
dicts = get_unit_dicts(units_path)
gs = globals()
for dict_ in dicts:
    gs.update(dict_)


unit_dict = {unit:abbrev for abbrev, unit in
             chain(units_si,
                   units_extra,
                   units_dimensionless,
                   units_imp,)}

prefix_dict = {prefix:abbrev for abbrev, prefix in prefixes_si}


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
        tup = self._tuple
        return TextPP(self)()


class TextPP:
    def __init__(self, pp):
        self.pp = pp
        self.tup = pp._tuple


    def name_to_python(self, first):
        return first.split(':', 1)[-1]

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
        print(first, pyfirst, rest)
        if isinstance(rest, list) or isinstance(rest, tuple):
            value = [self.eval(r) for r in rest]
        return getattr(self, pyfirst)(*value)  # apply is * woo

    def expr(self, tup):
        return tup
        #return self.eval(tup)

    def range(self, start, stop):
        #start, stop = self.eval(tup)
        #f'{start}-{stop}'
        #start, stop = tup
        return f'{start}-{stop}'
        #'-'.join((self.eval(start), self.eval(stop)))

    def unit(self, unit, prefix=None):
        p = self._prefix(prefix)
        u = self._unit(unit)
        return f'{p}{u}'

    def _prefix(self, prefix):
        if prefix:
            prefix = prefix[1:]
            return prefix_dict[prefix]
        else:
            return ''

    def _unit(self, unit):
        unit = unit[1:]
        return unit_dict[unit]

    def quantity(self, value, unit):
        #unit_value = self.eval(unit)  # FIXME eval should be smarter than this ...
        return f'{value}{unit}'

        
def _pprint_operation(self, object, stream, indent, allowance, context, level):
    #value = object.format_value(indent)  # how the heck does this work?
    value = object.__repr__(indent)  # how the heck does this work?
    stream.write(value)


pprint.PrettyPrinter._dispatch[ProtcParameter.__repr__] = _pprint_operation
