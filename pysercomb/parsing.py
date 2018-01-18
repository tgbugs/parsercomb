#!/usr/bin/env python3.6

import os
from pyontutils.utils import coln
from IPython import embed

infinity = 99999  # you know it baby, if we go this deep we will get recursion errors
__script_folder__ = os.path.dirname(os.path.realpath(__file__))

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
                    print('caught EOF in TIMES', True, matches, p)
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
                print('WAT')
                return True, None, p
    return not_

def END(func1, func2):
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

def RETUNBOX(v):
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

def param(prefix_name):
    name = 'param:' + prefix_name
    def add_function_type(v):
        return RETURN((name, *v))
    def paramed(parser_func):
        return BIND(parser_func, add_function_type)
    return paramed

def STRINGIFY(func):
    return transform_value(func, lambda v: '"' + str(v).replace('"', '\\"') + '"')

def AT_MOST_ONE(func, fail=True): return BIND(TIMES(func, 0, 1, fail), RETUNBOX)
#def AT_MOST_ONE(func, fail=True): return transform_value(TIMES(func, 0, 1, fail), lambda v: v[0] if v else v)

def EXACTLY_ONE(func, fail=True): return BIND(TIMES(func, 1, 1, fail), RETUNBOX)
#def EXACTLY_ONE(func, fail=True): return transform_value(TIMES(func, 1, 1, fail), lambda v: v[0] if v else v)

# I hate the people who felt the need to make different type blocks for this stuff in 1673
EN_DASH = b'\xe2\x80\x93'.decode()
MINUS_SIGN = b'\xe2\x88\x92'.decode()  # HAH CAUGHT YOU
HYPHEN_MINUS = b'\x2d'.decode()  # yes, the thing that most keyboards have
minus_sign = COMP(MINUS_SIGN)
en_dash = COMP(EN_DASH)
hyphen_minus = COMP(HYPHEN_MINUS)
_dash_thing = OR(hyphen_minus, en_dash, minus_sign)  # THERE ARE TOO MANY AND THEY ALL LOOK THE SAME
dash_thing = RETVAL(_dash_thing, HYPHEN_MINUS)
double_dash_thing = TIMES(dash_thing, 2)
thing_accepted_as_a_dash = transform_value(OR(double_dash_thing, dash_thing), lambda v: HYPHEN_MINUS)

# basic tokens and operators
space = COMP(' ')
spaces = MANY(space)
spaces1 = MANY1(space)
colon = COMP(':')
plus_or_minus_symbol = COMP('±')  # NOTE range and +- are interconvertable...
plus_or_minus_pair = COMP('+-')  # yes that is an b'\x2d'
plus_over_minus = COMP('+/-')
plus_or_minus = transform_value(OR(plus_or_minus_symbol, plus_or_minus_pair, plus_over_minus), lambda v: 'plus-or-minus')
CROSS = b'\xc3\x97'.decode()
cross = COMP(CROSS)
x = COMP('x')
by = OR(cross, x)
exponent = COMP('^')
addition = COMP('+')
subtraction = dash_thing
division = COMP('/')
multiplication = RETVAL(OR(COMP('*'), by), '*')  # safe since we added lookahead to dimensions
math_op = OR(addition, subtraction, division, multiplication, exponent)  # FIXME subtraction is going to be a pain
unit_op = OR(division, multiplication)
lt = COMP('<')
lte = COMP('<=')
gt = COMP('>')
gte = COMP('>=')
comparison = OR(lte, gte, lt, gt)
approx = RETVAL(COMP('~'), 'approximately')

# number words
_numlookup = {
    'zero':0,
    'one':1,
    'two':2,
    'three':3,
    'four':4,
    'five':5,
    'six':6,
    'seven':7,
    'eight':8,
    'nine':9, }
num_word_lower = OR(*make_funcs(_numlookup, _numlookup))
def num_word_cap(p): return num_word_lower(p.lower())
num_word = OR(num_word_lower, num_word_cap)

# numbers
digits = [str(_) for _ in range(10)]
def digit(p): return oper(p, lambda d: d in digits)
point = COMP('.')
def char(p): return oper(p, lambda c: c.isalpha())
_int_ = JOINT(TIMES(dash_thing, 0, 1), MANY1(digit), join=True)
int_ = transform_value(_int_, lambda i: int(''.join(i)))
_float_ = JOINT(TIMES(dash_thing, 0, 1),
                OR(JOINT(MANY1(digit), point, MANY(digit), join=True),
                   JOINT(MANY(digit), point, MANY1(digit), join=True)),
                join=True)
float_ = transform_value(_float_, lambda f: float(''.join(f)))
E = COMP('E')
times = COMP('*')
exponental_notation = JOINT(OR(float_, int_),  # FIXME not including as a num for now because it is sometimes used distributively across plust-or-minus infix
                            COMPOSE(spaces, OR(by, times)),
                            COMPOSE(spaces, COMP('10')),
                            exponent, int_)
_scientific_notation = joinstr(JOINT(joinstr(OR(_float_, _int_)), E, joinstr(_int_)))
scientific_notation = _scientific_notation  # BIND(_scientific_notation, lambda v: RETURN(float(v)))
num = OR(scientific_notation, float_, int_, num_word)  # float first so that int doesn't capture it

# racket
def exp(p):
    return _exp(p)
newline = COMP('\n')
whitespace_atom = OR(COMP(' '), COMP('\t'), newline)
#whitespace_atom = OR(_whitespace_atom, END(_whitespace_atom, EOF))
whitespace = MANY(whitespace_atom)
whitespace1 = MANY1(whitespace_atom)  # FIXME this is broken to negation? (extremely slow)
comment = COMPOSE(whitespace,
                  COMPOSE(COMP(';'),
                          MANY(NOT(newline))))  # leave the newline intact
def LEXEME(func):
    return COMPOSE(OR(SKIP(comment, whitespace), whitespace), SKIP(func, OR(comment, whitespace)))
open_paren = LEXEME(COMP('('))
close_paren = LEXEME(COMP(')'))
quote_symbol = COMP("'")
double_quote_symbol = COMP('"')
_string = COMPOSE(double_quote_symbol,
                  SKIP(MANY(NOT(double_quote_symbol)),
                       double_quote_symbol))  # TODO escape
string = LEXEME(joinstr(_string))
symbol = OR(char, digit, COMP('-'), COMP('_'), colon, COMP('*'),
            NOT(OR(COMP('('),
                   COMP(')'),
                   quote_symbol,
                   double_quote_symbol,
                   COMP(';'),
                   whitespace1,
                   point,
                   EOF)))
NIL = COMP("'()")
num_literal = OR(scientific_notation, float_, int_)
cons_pair = COMPOSE(open_paren, JOINT(SKIP(exp, point), SKIP(exp, close_paren)))
literal = OR(string, num_literal, cons_pair, NIL)
atom = joinstr(MANY1(symbol))
identifier = LEXEME(atom)
def _quote(p):
    return COMPOSE(quote_symbol, _exp)(p)
quote = LEXEME(_quote)
def sexp(p):
    return sexp_inner(p)
_exp = LEXEME(OR(identifier, quote, literal, sexp, NIL))
sexp_inner = COMPOSE(open_paren, SKIP(MANY1(exp), close_paren))
lang_line = JOINT(COMP('#lang'), SKIP(MANY(NOT(COMP('\n'))), COMP('\n')))
racket_doc = COMPOSE(AT_MOST_ONE(lang_line), MANY(exp))
tag_doc = SKIP(JOINT(COMPOSE(open_paren,
                             COMP('tag-doc')),
                     quote,
                     string),
               close_paren)
tag_docs = MANY1(tag_doc)

# units
def get_quoted_list(filename):
    with open(os.path.expanduser('~/ni/dev/protocols/rkt/units/' + filename), 'rt') as f:
        src = f.read()
        success, value, rest = racket_doc(src)
    if not success:
        raise SyntaxError(f'Something is wrong in {filename}. Parse output:\n{value}\n\n{rest}')
    out = {}
    for expression in value:
        #print(expression)
        if expression[0] == 'define':
            name = expression[1].replace('-','_')
            out[name] = expression[2]
    return out

dicts = [get_quoted_list(_) for _ in ('si-prefixes-data.rkt', 'si-prefixes-exp-data.rkt', 'si-units-data.rkt', 'si-units-extras.rkt', 'units-dimensionless.rkt')]
gs = globals()
for dict_ in dicts:
    gs.update(dict_)

_silookup = {k: "'" + v for k, v in units_si + units_extra + tuple([v, v] for k, v in units_si) + tuple([v, v] for k, v in units_extra)}
_siplookup = {k: "'" + v for k, v in prefixes_si}

siprefix = OR(*make_funcs(coln(0, prefixes_si), _siplookup))
siunit = OR(*make_funcs(list(coln(0, units_si + units_extra)) + # need both here to avoid collisions in unit_atom slower but worth it?
                        list(coln(1, units_si + units_extra)), _silookup))

DEGREES_UNDERLINE = b'\xc2\xba'.decode()  # º sometimes pdfs misencode these
DEGREES_FEAR = b'\xe2\x97\xa6' # this thing is scary and I have no id what it is or why it wont change color ◦
_C_for_temp = COMP('C')
C_for_temp = RETVAL(_C_for_temp, BOX(_silookup['degrees-celcius']))
temp_for_biology = JOINT(num, C_for_temp, join=False)

unit_atom = param('unit')(BIND(OR(JOINT(siprefix, siunit, join=False),
                                  BIND(siunit, RETBOX)),
                               FLOP))

maybe_exponent = AT_MOST_ONE(exponent)

def cull_empty(return_value):
    if return_value and not any(return_value[1:]):
            return RETURN(return_value[0])
    return RETURN(return_value)

def flatten(return_value):
    first, rest = return_value
    return RETURN((first, *rest))

def flatten1(return_value):
    return RETURN(tuple(t for r in return_value for t in r))

def op_order(return_value):
    order = 'plus-or-minus', 'range', '^', '/', '*', '+', '-'
    associative = '*', '+'
    commutative = '*', '+'
    def inner(subtree):
        for op in order[::-1]:
            try:
                i = subtree.index(op)
                front = subtree[:i]
                rest = subtree[i + 1:]
                f = inner(front)
                r = inner(rest)
                if op in commutative and len(front) >= len(rest):
                    f, r = r, f
                if op in associative:
                    if f[0] == op and r[0] == op:
                        return (op, f[1:], r[1:])
                    elif f[0] == op:
                        return (op, f[1:], r)
                    elif r[0] == op:
                        return (op, f, r[1:])
                return (op, f, r)
            except ValueError:
                continue
        return subtree[0]
    lisped = inner(return_value)
    if lisped != return_value:
        return RETURN((lisped,))
    else:
        return RETURN(return_value)

unit_suffix = OR(JOINT(COMPOSE(spaces, unit_op),
                    COMPOSE(spaces, unit_atom)),
                 JOINT(COMPOSE(spaces, maybe_exponent),
                       int_))
unit_expression = param('unit-expr')(BIND(BIND(JOINT(unit_atom,
                                                           BIND(MANY1(unit_suffix),
                                                                flatten1)),
                                                     flatten),
                                                op_order))
unit_shorthand = param('unit-expr')(BIND(JOINT(unit_atom,
                                               COMPOSE(spaces,
                                                       BIND(JOINT(unit_atom, int_),
                                                            lambda v: RETURN(('^', *v))))),
                                         lambda v: RETBOX(('*', *v))))
unit = OR(unit_expression, unit_shorthand, unit_atom)
unit_starts_with_dash = COMPOSE(dash_thing, unit)  # derp
unit_implicit_count_ratio = BIND(JOINT(LEXEME(division),
                                       unit,
                                       join=False),
                                 lambda v: RETBOX((v[0], unit("count")[1], *v[1:])))

def plus_or_minus_thing(thing): return JOINT(plus_or_minus, COMPOSE(spaces, thing), join=False)

to = COMP('to')
range_indicator = transform_value(OR(thing_accepted_as_a_dash, to), lambda v: 'range')
def range_thing(func): return JOINT(func, COMPOSE(spaces, range_indicator), COMPOSE(spaces, func))

pH = RETVAL(COMP('pH'), BOX("'pH"))
P = COMP('P')
post_natal_day = RETVAL(P, BOX("'postnatal-day"))  # FIXME note that in our unit hierarchy this is a subclass of days
_fold_prefix = END(by, num)
fold_prefix = RETVAL(_fold_prefix, BOX("'fold"))

prefix_unit = param('prefix-unit')(OR(pH, post_natal_day, fold_prefix))
_prefix_quantity = JOINT(prefix_unit, COMPOSE(spaces, num))  # OR(JOINT(fold, num))
prefix_quantity = BIND(_prefix_quantity, FLOP)

_percent = COMP('%')
percent = RETVAL(_percent, BOX("'percent"))
fold_suffix = RETVAL(END(by, noneof('0123456789')), BOX("'fold"))  # NOT(num) required to prevent issue with dimensions
_suffix_unit = param('unit')(OR(percent, unit_implicit_count_ratio))
suffix_unit = OR(_suffix_unit, unit)
suffix_unit_no_space = param('unit')(OR(EXACTLY_ONE(fold_suffix), C_for_temp, unit_starts_with_dash))  # FIXME this is really bad :/ and breaks dimensions...
suffix_quantity = JOINT(num, OR(suffix_unit_no_space,
                                COMPOSE(spaces,
                                        AT_MOST_ONE(suffix_unit, fail=False))))  # this catches the num by itself and leaves a blank unit
quantity = param('quantity')(OR(prefix_quantity, suffix_quantity))

dilution_factor = param('dilution')(JOINT(SKIP(int_, colon), int_, join=False))
sq = COMPOSE(spaces, quantity)
sby = COMPOSE(spaces, by)
dimensions = param('dimensions')(BIND(JOINT(quantity,
                                       MANY1(COMPOSE(COMPOSE(spaces,
                                                             SKIP(by,
                                                                  spaces)),
                                                     END(quantity, NOT(exponent))))),  # catch A x B^C
                                      flatten))
prefix_operator = OR(approx, plus_or_minus, comparison)
infix_operator = OR(plus_or_minus, range_indicator, math_op)  # colon? doesn't really operate on quantities, note that * and / do not interfere with the unit parsing because that takes precedence
infix_suffix = JOINT(COMPOSE(spaces, infix_operator),
                     COMPOSE(spaces, quantity))
infix_expression = BIND(BIND(JOINT(quantity,
                              BIND(MANY1(infix_suffix),
                                   flatten1)),
                             flatten),
                        op_order)
prefix_expression = BIND(BIND(JOINT(prefix_operator,
                               COMPOSE(spaces,
                                       OR(infix_expression,
                                          quantity))),
                         flatten), RETBOX)
expression = param('expr')(OR(prefix_expression, infix_expression))  # FIXME this doesn't work if you have prefix -> infix are there cases that can happen?

#def approximate_thing(thing): return JOINT(EXACTLY_ONE(approx), COMPOSE(spaces, thing), join=False)

def FAILURE(p):
    return param('parse-failure')(lambda null: (True, tuple(), p))(p)

# TODO objective specifications...
components = OR(dimensions,
                dilution_factor,
                expression,
                quantity,
                FAILURE)
#approx_comp = approximate_thing(components)

parameter_expression = components#OR(approx_comp, components)

def main():
    from desc.prof import profile_me
    from time import time

    print(identifier('hello world!'))
    print(quote('\'hello world!'))
    print(quote('\'"hello" world!'))
    print(string('"ARE YOU KIDDING ME \n NO???"'))
    print(tag_doc('(tag-doc \'butts "wat wat wat")\n'))
    with open(os.path.expanduser('~/git/protc/protc-tags.rkt'), 'rt') as f:
        text = f.read()
    with open(os.path.expanduser('~/ni/dev/protocols/rkt/units/si-units-extras.rkt'), 'rt') as f:
        text2 = f.read()
    td = tag_docs(text)
    e = racket_doc(text2)

    tests = ('1 daL', "300 mOsm", "0.5 mM", "7 mM", "0.1 Hz.", "-50 pA",
             "200–500mm", "0.3%–0.5%", "1:500", "4%", "10 U/ml",
             "–20°C", "<10 mV", "–70 ± 1 mV", "30 to 150 pA",
             "310 mosmol/l", "13–16 days old", "50 x 50 um",
             "~3.5 - 6 Mohms", "pH 7.3", "17–23 d old", "10 –100",
             "250 +- 70 um", "20±11 mm", "+- 20 degrees",
             '0.1 mg kg–1', '75  mg / kg', '40x', 'x100',
             '200μm×200μm×200μm', '20--29 days', '4 °C', '10×10×10',
             '10 kg * mm^2 / s^2', '10 * 1.1 ^ 30 / 12',
             '120 +- 8 * 10 ^ 6 MR / kg * s2 * 20',
             '1 * 2 * 3 * 4 * 5', '1 + 2 + 3 + 4 + 5',
            )
    weirds = ("One to 5", "100-Hz", "25 ng/ul)", "34–36°C.",
              '3*10^6 infectious particles/mL',
              '4.7 +- 0.6 x 10^7 / mm^3',  # FIXME this is ambigious? YES VERY also unit dimensionality...
              '1,850', '4C', 'three', 'Four', 'P28.5±2 days'
             )
    should_fail = ('~~~~1',
                   "(pH 7.3",
                  )
    with open(os.path.expanduser('~/ni/dev/protocols/rkt/test-params.rkt'), 'rt') as f:
        success, v, rest = racket_doc(f.read())#[l.strip().strip('"') for l in f.readlines()][3:-1]
    param_test_strings = [s for e in v  if e[0] == 'define' and e[1] == 'param-test-strings' for s in e[2]]
    test_all = []

    def timeit():
        for t in param_test_strings:
            success = False
            t2 = t
            while t2 and not success:
                _, v, rest = parameter_expression(t2)
                success = v[0] != 'param:parse-failure'
                if not success:
                    t2 = t2[1:]
            if not success:
                rest = t
            test_all.append((success, v, rest))
    start = time()
    timeit()
    stop = time()
    print('BAD TIME', stop - start)

    q = "'"
    fun = [t.split(' ')[-1] for t in tests][:5]
    test_unit_atom = [unit_atom(f) for f in fun]
    test_unit = [unit(f) for f in fun]
    test_quantity = [quantity(t) for t in tests]
    test_expression = [parameter_expression(t) for t in tests + weirds]
    test_expression2 = '\n'.join(sorted((f"'{t+q:<25} -> {parameter_expression(t)[1]}" for t in tests + weirds), key=lambda v: v[25:]))
    print(test_expression2)
    test_fails = [parameter_expression(t) for t in tests]
    embed()

if __name__ == '__main__':
    main()
    
