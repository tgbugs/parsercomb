#!/usr/bin/env python3.6

import os
from .utils import log

infinity = 99999  # you know it baby, if we go this deep we will get recursion errors
__script_folder__ = os.path.dirname(os.path.realpath(__file__))

__all__ = [
    'ANDTHEN',
    'AT_MOST_ONE',
    'BIND',
    'BOX',
    'COMP',
    'COMPOSE',
    'END',
    'EOF',
    'EXACTLY_ONE',
    'FLOP',
    'JOINT',
    'MANY',
    'MANY1',
    'NOT',
    'OR',
    'RETURN',
    'RETURNBOX',
    'RETVAL',
    'RETBOX',
    'SKIP',
    'NOOP',

    'noneof',

    'make_funcs',
    'joinstr',
    'transform_value',
    'flatten',
    'flatten1',
    'noop',

    'spaces',
    'newline',
    'whitespace',
    'whitespace1',

    'dash_thing',
    'thing_accepted_as_a_dash',

    'by',
    'char',
    'colon',
    'digit',
    'point',
    'exponent',
    'scientific_notation',
    'float_',
    'int_',
    'num',
    'boolean',
]


# helper types

class ScientificNotation(float):
    """ type for perserving the original format of scientific notation """
    def __new__(cls, value):
        self = super().__new__(cls, value)
        self.__original = value
        return self

    def __repr__(self):
        return self.__original


# utiltiy

def wdebug(func):
    def wrapped(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except BaseException as e:
            breakpoint()
            raise e

    return wrapped

# combinators

def RETURN(v):
    def return_(p):
        return True, v, p
    return return_

def OR(*funcs):
    def or_(p):
        for f in funcs:
            success, v, rest = f(p)
            if success:
                return success, v, rest
        return False, None, p  # explicit values since different funcs will have parsed to different depths
    return or_

def TIMES(func, min_, max_=None, fail=True):
    if not max_ and not min_:
        raise TypeError('If you want something zero times use NOT')
    def times(p):
        matches = []
        for i in range(min_):
            success, v, rest = func(p)
            if not success:
                return success, None, p
            else:
                matches.append(v)
                p = rest
        if max_ is not None:
            for i in range(max_ - min_):
                try:
                    success, v, rest = func(p)
                except EOFError:  # NOT(anything) matches EOF
                    log.debug(('caught EOF in TIMES', True, matches, p))
                    return True, tuple(matches), p
                if not success:
                    return True, tuple(matches), p
                else:
                    matches.append(v)
                    p = rest
            if fail:
                success, v, rest = func(p)
                if success:
                    return False, None, p
                else:
                    success = True
                    rest = p

        return success, tuple(matches), rest
    return times

def MANY(func):
    return TIMES(func, 0, infinity)

def MANY1(func):
    return TIMES(func, 1, infinity)

def ANDTHEN(func1, func2):
    """ match func1 func2 | rest
        (func1 func2) -> """
    def andthen(p):
        success, v1, rest = func1(p)
        if success:
            p2 = rest
            success, v2, rest = func2(p2)
            if success:
                return success, (v1, v2), rest
        return success, None, p
    return andthen

def JOINT(*funcs, join=False):  # FIXME something is up with multichar tokens when join=True
    def joint(p):
        matches = []
        rest = p
        for func in funcs:
            success, v, rest = func(rest)
            if not success:
                return success, None, p
            else:
                if join and hasattr(v, '__iter__'):
                    matches.extend(v)
                else:
                    matches.append(v)
        return success, tuple(matches), rest 
    return joint

def JOINT_OR_FIRST(*funcs, join=False):  # TODO improve performance in cases where there is a base that should return true even when the rest fails
    pass

def COMPOSE(func1, func2):
    """ match func1 func2 | rest
        func1 -x
        func2 -> value """
    def compose(p):
        success, v, rest = func1(p)
        if success:
            success, v, rest = func2(rest)
            if success:
                return success, v, rest
        return success, None, p
    return compose

def BIND(parser, func2):
    def bind(p):
        success, v , rest = parser(p)
        if success:
            parser2 = func2(v)
            success, v, rest = parser2(rest)
            if success:
                return success, v, rest
        return False, v, p
    return bind

def NOT(func):
    def not_(p):
        success, v, rest = func(p)
        if success:
            return False, v, rest
        else:
            if p:
                return True, p[0], p[1:]
            else:
                raise EOFError(f'Unhandled EOF received by NOT in {func} {func.__name__}.\n'
                               'You may have a MANY->NOT case.')
                log.debug('WAT')
                return True, None, p
    return not_

def END(func1, func2):
    """ this is a form of lookahead """
    def end_(p):
        success, v, rest = func1(p)
        if not success:
            return success, v, rest
        try:
            success2, v2, rest2 = func2(rest)
        except EOFError:  # NOT will match EOF
            return success, v, rest
        if success2:
            return success, v, rest
        else:
            return success2, v2, rest2
    return end_

def SKIP(func1, func2):
    """ match func1 func2 | rest
        func1 -> value
        func2 -x """
    def skip(p):
        success, v, rest = func1(p)
        if not success:
            return success, v, rest
        success2, v2, rest2 = func2(rest)
        if success2:
            return success2, v, rest2
        else:
            return success2, v2, rest2
    return skip


def NOOP(func):
    return (lambda p: (True, None, p))


def comp(p, val, lv):
    if p:
        v = p[:lv]  # warning: this will produce order dependencies when you spec the parser
    else:
        return False, None, p  # we are at the end
    return v == val, v, p[lv:]

def comp1(p, val):
    if p:
        v = p[0]  # warning: this will produce order dependencies when you spec the parser
    else:
        return False, None, p  # we are at the end
    return v == val, v, p[1:]

def oper(p, func):
    if p:
        v = p[0]
    else:
        return False, None, p  # we are at the end
    return func(v), v, p[1:]

def noneof(string):
    def noneof_(p):
        if not p:
            return True, p, p
        for s in string:
            success, v, rest = comp(p, s, 1)
            if success:
                return False, v, p
        return True, p[0], p[1:]
    return noneof_

def COMP(val):
    lv = len(val)
    if lv == 1:
        def comp1_(p):
            return  comp1(p, val)
        comp1_.__name__ = 'COMP_' + val
        return comp1_
    else:
        def comp_(p):
            return comp(p, val, lv)
        comp_.__name__ = 'COMP_' + val
        return comp_

def EOF(p):
    if p == '':
        return True, '', ''
    else:
        return False, None, p

#
# function to allow implementation of what the parser actually does/outputs

def BOX(v):
    return v,

def RETBOX(v):
    return RETURN((v,))

def RETURNBOX(v):
    if v:
        v, = v
    return RETURN(v)


def FLOP(return_value):
    return RETURN(tuple(return_value[::-1]))

def RETVAL(func, val):
    #def return_bound_val(v):
        #return RETURN(val)
    #return BIND(func, return_bound_val)
    return COMPOSE(func, RETURN(val))

def transform_value(parser_func, func_to_apply):
    def transformed(p):
        success, value, rest = parser_func(p)
        if success:
            return success, func_to_apply(value), rest
        else:
            return success, value, rest
    return transformed

def make_funcs(inpt, lookuptable):
    for token in sorted(inpt, key=lambda a: -len(a)):  # sort to simulate right associativity (ie da recognized even if d a token)
        def lookup_function(v):
            return RETURN(lookuptable[v])
        yield BIND(COMP(token), lookup_function)

def jstring(v): return RETURN(''.join(v))

def joinstr(func):
    return BIND(func, jstring)

def STRINGIFY(func):
    return transform_value(func, lambda v: '"' + str(v).replace('"', '\\"') + '"')

def AT_MOST_ONE(func, fail=True): return BIND(TIMES(func, 0, 1, fail), RETURNBOX)
#def AT_MOST_ONE(func, fail=True): return transform_value(TIMES(func, 0, 1, fail), lambda v: v[0] if v else v)

def EXACTLY_ONE(func, fail=True): return BIND(TIMES(func, 1, 1, fail), RETURNBOX)
#def EXACTLY_ONE(func, fail=True): return transform_value(TIMES(func, 1, 1, fail), lambda v: v[0] if v else v)

# I hate the people who felt the need to make different type blocks for this stuff in 1673
HYPHEN = b'\xe2\x80\x90'.decode()  # Y U DO DIS unicode 2010
EN_DASH = b'\xe2\x80\x93'.decode()
MINUS_SIGN = b'\xe2\x88\x92'.decode()  # HAH CAUGHT YOU aka u2212
HYPHEN_MINUS = b'\x2d'.decode()  # yes, the thing that most keyboards have
hyphen = COMP(HYPHEN)
minus_sign = COMP(MINUS_SIGN)
en_dash = COMP(EN_DASH)
hyphen_minus = COMP(HYPHEN_MINUS)
_dash_thing = OR(hyphen_minus, en_dash, minus_sign, hyphen)  # THERE ARE TOO MANY AND THEY ALL LOOK THE SAME
dash_thing = RETVAL(_dash_thing, HYPHEN_MINUS)
double_dash_thing = TIMES(dash_thing, 2)
thing_accepted_as_a_dash = BIND(OR(double_dash_thing, dash_thing), lambda v: RETURN(HYPHEN_MINUS))

# basic tokens and operators
thin_space = COMP('\u2009')  # sigh what bloodyminded program would ever produce these!!??!
non_breaking_space = COMP('\xa0')  # nbsp
space = OR(COMP(' '), non_breaking_space, thin_space)
spaces = MANY(space)
spaces1 = MANY1(space)
colon = COMP(':')
exponent = COMP('^')
point = COMP('.')
dot = COMP('·')  # b'\xc2\xb7' MIDDLE DOT U+00B7

newline = COMP('\n')
whitespace_atom = OR(COMP(' '), COMP('\t'), newline)
#whitespace_atom = OR(_whitespace_atom, END(_whitespace_atom, EOF))
whitespace = MANY(whitespace_atom)
whitespace1 = MANY1(whitespace_atom)  # FIXME this is broken to negation? (extremely slow)

CROSS = b'\xc3\x97'.decode()
cross = COMP(CROSS)
x = COMP('x')
by = OR(cross, x)

# booleans
_boollookup = {
    'true': True,
    'false': False,
    '#t': True,
    '#f': False,}
bool_word_lower = OR(*make_funcs(_boollookup, _boollookup))
def bool_word_cap(p): return bool_word_lower(p.lower())
boolean = OR(bool_word_lower, bool_word_cap)

# number words
_numlookup = {
    'zero': 0,
    'one': 1,
    'two': 2,
    'three': 3,
    'four': 4,
    'five': 5,
    'six': 6,
    'seven': 7,
    'eight': 8,
    'nine': 9,
    'ten': 10,
    'twenty': 20,
    'thirty': 30,
    'forty': 40,
    'fifty': 50,
    'sixty': 60,
    'seventy': 70,
    'eighty': 80,
    'ninety': 90,
    # going beyond this requires writing a look-ahead parser
    # for natural language numbers, which is out of scope atm
}
num_word_lower = OR(*make_funcs(_numlookup, _numlookup))
def num_word_cap(p): return num_word_lower(p.lower())
num_word = OR(num_word_lower, num_word_cap)

# numbers
digits = [str(_) for _ in range(10)]
def digit(p): return oper(p, lambda d: d in digits)
def char(p): return oper(p, lambda c: c.isalpha())
_int_ = JOINT(TIMES(dash_thing, 0, 1), MANY1(digit), join=True)
int_ = transform_value(_int_, lambda i: int(''.join(i)))
_float_ = JOINT(TIMES(dash_thing, 0, 1),
                OR(JOINT(MANY1(digit), point, MANY(digit), join=True),
                   JOINT(MANY(digit), point, MANY1(digit), join=True)),
                join=True)
float_ = transform_value(_float_, lambda f: float(''.join(f)))
E = OR(COMP('E'), COMP('e'))
times = COMP('*')
exponental_notation = JOINT(OR(float_, int_),  # FIXME not including as a num for now because it is sometimes used distributively across plust-or-minus infix
                            COMPOSE(spaces, OR(by, times)),
                            COMPOSE(spaces, COMP('10')),
                            exponent, int_)
_scientific_notation = joinstr(JOINT(joinstr(OR(_float_, _int_)), E, joinstr(_int_)))
scientific_notation = BIND(_scientific_notation, lambda v: RETURN(ScientificNotation(v)))
num = OR(scientific_notation, float_, int_, num_word)  # float first so that int doesn't capture it

def cull_empty(return_value):
    if return_value and not any(return_value[1:]):
            return RETURN(return_value[0])
    return RETURN(return_value)

def flatten(return_value):
    first, rest = return_value
    return RETURN((first, *rest))

def flatten1(return_value):
    return RETURN(tuple(t for r in return_value for t in r))

def noop(function):
    return lambda return_value: RETURN(return_value)
