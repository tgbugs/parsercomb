#!/usr/bin/env python3

import parsec
import os
from pysercomb.utils import coln
from IPython import embed

def param(symbol):
    def decorator(parser):
        return parser.parsecmap(lambda v: ('param:' + symbol, *v))
    return decorator

def BOX(v):
    return v,

def OR(parser, *parsers):
    if not parsers:
        return parser
    return parsec.try_choice(parser, OR(*parsers))

DEGREES_UNDERLINE = b'\xc2\xba'.decode()  # º sometimes pdfs misencode these
DEGREES_FEAR = b'\xe2\x97\xa6' # this thing is scary and I have no id what it is or why it wont change color ◦
def get_quoted_list(filename):
    with open(os.path.expanduser('~/git/protc/protc-lib/protc/units/' + filename), 'rt') as f:
        lines = [_.split(';')[0].strip() for _ in f.readlines()][3:-1]
    return [line.strip("'").strip('(').rstrip(')').split(' . ') for line in lines if line and '.' in line]

_SIPREFS, _SIEXPS, _SIUNITS, _EXTRAS = [get_quoted_list(_) for _ in ('si-prefixes-data.rkt', 'si-prefixes-exp-data.rkt', 'si-units-data.rkt', 'si-units-extras.rkt')]
_silookup = {k: "'" + v for k, v in
             _SIUNITS + _EXTRAS +
             [[v, v] for k, v in _SIUNITS] +
             [[v, v] for k, v in _EXTRAS]}
_siplookup = {k: "'" + v for k, v in _SIPREFS}

def make_funcs(inpt, lookuptable):
    args = []
    for token in sorted(inpt, key=lambda a: -len(a)):  # right associativity (ie da recognized even if d a token)
        args.append(parsec.string(token).parsecmap(lambda v: lookuptable[v]))
    return args

space = parsec.space()
spaces = parsec.spaces()
spaces1 = parsec.many1(space)
carrot = parsec.string('^')
exponent = carrot.parsecmap(lambda v: 'exponent')
division = parsec.string('/')
multiplication = parsec.string('*')
unit_op = division ^ multiplication
colon = parsec.string(':')
plus_or_minus_symbol = parsec.string('±')
plus_or_minus_pair = parsec.string('+-')  # yes that is an b'\x2d'
plus_or_minus = OR(plus_or_minus_symbol, plus_or_minus_pair).parsecmap(lambda v: 'plus-or-minus')
# I hate the people who felt the need to make different type blocks for this stuff in 1673
EN_DASH = b'\xe2\x80\x93'.decode()
HYPHEN_MINUS = b'\x2d'.decode()  # yes, the thing that most keyboards have
en_dash = parsec.string(EN_DASH)
hyphen_minus = parsec.string(HYPHEN_MINUS)
dash_thing = (en_dash ^ hyphen_minus).parsecmap(lambda v: HYPHEN_MINUS)  # THERE ARE TOO MANY AND THEY ALL LOOK THE SAME
double_dash_thing = parsec.times(dash_thing, 2)
thing_accepted_as_a_dash = OR(double_dash_thing, dash_thing).parsecmap(lambda v: HYPHEN_MINUS)
lt = parsec.string('<')
lte = parsec.string('<=')
gt = parsec.string('>')
gte = parsec.string('>=')
comparison = OR(lte, gte, lt, gt)
CROSS = b'\xc3\x97'.decode()
cross = parsec.string(CROSS)
x = parsec.string('x')
by = OR(cross, x)
approx = parsec.string('~').parsecmap(lambda v: 'approximately')
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
@parsec.Parser
def num_word_cap(text, index):
    return num_word_lower(text.lower(), index)
num_word = num_word_lower ^ num_word_cap

# note that the behavior of these differes from mine in that they do not fail if we exceed max
def AT_MOST_ONE(parser): return parsec.times(parser, 0, 1).parsecmap(lambda v: v[0] if v else '')
def EXACTLY_ONE(parser): return parsec.times(parser, 1, 1).parsecmap(lambda v: v[0])

def join(v): return ''.join(v)

digit = parsec.digit()
digits = parsec.many(digit).parsecmap(lambda v: ''.join(v))
digits1 = parsec.many1(digit).parsecmap(lambda v: ''.join(v))
point = parsec.string('.')
_int_ = (AT_MOST_ONE(dash_thing) + digits1).parsecmap(join)
int_ = _int_.parsecmap(int)
_float_ = parsec.joint(AT_MOST_ONE(dash_thing),
                       parsec.try_choice(parsec.joint(digits1, point, digits).parsecmap(join),
                                         parsec.joint(digits, point, digits1).parsecmap(join))
                      ).parsecmap(join)
float_ = _float_.parsecmap(float)
E = parsec.string('E')
times = parsec.string('*')
scientific_notation = parsec.joint((_float_ ^ _int_), E, _int_).parsecmap(join).parsecmap(float)
num = OR(scientific_notation,
         float_,  # float first so int can't capture
         int_,
         num_word)
#num = parsec.regex(r'-?(0|[1-9][0-9]*)([.][0-9]+)?([eE][+-]?[0-9]+)?')
siprefix = OR(*make_funcs(coln(0, _SIPREFS), _siplookup))
siunit = OR(*make_funcs(list(coln(0, _SIUNITS + _EXTRAS)) +
                        list(coln(1, _SIUNITS + _EXTRAS)), _silookup))
@parsec.generate
def unit_atom():
    unit = yield (siprefix + siunit).parsecmap(lambda v: (v[1], v[0])) ^ siunit.parsecmap(BOX)
    return unit

@parsec.generate
def unit_base():
    atom = yield unit_atom << spaces
    exp = yield AT_MOST_ONE(AT_MOST_ONE(exponent).parsecmap(lambda v: 'exponent') + (spaces >> int_))
    if exp:
        return atom, exp
    return atom

@parsec.generate
def unit():
    u = yield unit_base
    op = yield spaces >> AT_MOST_ONE(unit_op)
    more = yield parsec.many(spaces >> unit)
    if op and more:
        return (u, op, *more)
    elif op:
        return u, op
    elif more:
        return (u, '*', *more)  # no operator interpreted as multiplication eg kgm/s2
    else:
        return u

to = parsec.string('to')
range_indicator = (thing_accepted_as_a_dash ^ to).parsecmap(lambda v: 'range')
ph = parsec.string('pH')
P = parsec.string('P')
post_natal_day = P.parsecmap(lambda v: "'postnatal-days")
fold_prefix = parsec.ends_with(by, num).parsecmap(lambda v: "'fold")
prefix_unit = param('prefix-unit')(OR(ph, post_natal_day, fold_prefix).parsecmap(BOX))
prefix_quantity = (prefix_unit + (spaces >> num)).parsecmap(lambda v: (v[1], v[0]))
percent = parsec.string('%').parsecmap(lambda v: "'percent").parsecmap(BOX)
fold_suffix = parsec.ends_with(by.parsecmap(lambda v: "'fold"), parsec.eof() ^
                               parsec.none_of('1234567890')).parsecmap(BOX)
suffix_unit_no_space = param('unit')(EXACTLY_ONE(fold_suffix))
suffix_unit = param('unit')(percent ^ unit)
@parsec.generate
def suffix_quantity():
    v = yield num
    u = yield suffix_unit_no_space ^ (spaces >> AT_MOST_ONE(suffix_unit))
    return v, u if u else tuple()

quantity = param('quantity')(prefix_quantity ^ suffix_quantity)

@param('dilution')
@parsec.generate
def dilution_factor():
    one = yield int_
    yield colon
    times = yield int_
    return one, times

@param('dimensions')
@parsec.generate
def dimensions():
    first = yield quantity
    yield spaces >> by
    middle = yield parsec.many((spaces >> quantity) << (spaces >> by))
    last = yield spaces >> quantity
    return (first, *middle, last)

prefix_operator = plus_or_minus ^ comparison
infix_operator = OR(plus_or_minus, range_indicator, multiplication, division, exponent)
prefix_expression = prefix_operator + (spaces >> quantity)

@parsec.generate
def infix_expression():
    start = yield quantity << spaces
    op = yield infix_operator << spaces
    end = yield infix_expression ^ quantity.parsecmap(BOX)
    return (start, op, *end)

expression = param('expression')(prefix_expression ^ infix_expression)

def approximate_thing(parser):
    return EXACTLY_ONE(approx) + (spaces >> parser)

C_for_temp = param('unit')(parsec.string('C').parsecmap(lambda v: (_silookup['degrees-celcius'],)))
temp_for_biology = num + C_for_temp

@parsec.generate
def parameter_expression():
    aval = yield AT_MOST_ONE(approx << spaces)
    value = yield OR(dimensions,
                     dilution_factor,
                     temp_for_biology,
                     expression,
                     quantity,
                    )
    if aval:
        return aval, value
    return value

@parsec.Parser
def FAILED(text, index):
    return parsec.Value.success(len(text), ('parse-failure', text))

def main():
    tests = ('1 daL', "300 mOsm", "0.5 mM", "7 mM", "0.1 Hz.", "-50 pA",
             "200–500mm", "0.3%–0.5%", "1:500", "4%", "10 U/ml",
             "–20°C", "<10 mV", "–70 ± 1 mV", "30 to 150 pA",
             "310 mosmol/l", "13–16 days old", "50 x 50 um",
             "~3.5 - 6 Mohms", "pH 7.3", "17–23 d old", "10 –100",
             "250 +- 70 um", "20±11 mm", "+- 20 degrees",
             '0.1 mg kg–1', '75  mg / kg', '40x', 'x100',
             '200μm×200μm×200μm', '20--29 days', '4 °C', '10×10×10',
             '10 kg * mm^2 / s^2', '10 * 1.1 ^ 30 / 12'
            )
    weirds = ("One to 5", "100-Hz", "25 ng/ul)", "34–36°C.",
              '3*10^6 infectious particles/mL',
              '4.7 +- 0.6 x 10^7 / mm^3',  # FIXME this is ambigious? YES VERY also unit dimensionality...
              '1,850', '4C', 'three', 'Four', 'P28.5±2 days'
             )
    should_fail = ('~~~~1',
                   "(pH 7.3",
                  )
    #fun = [t.split(' ')[-1] for t in tests][:5]
    #test_unit_atom = [unit_atom(f) for f in fun]
    #test_unit = [unit(f) for f in fun]
    #test_quantity = [quantity(t) for t in tests]
    q = "'"
    test_expression = '\n'.join(f"'{t+q:<25} -> {parameter_expression(t, 0).value}" for t in tests + weirds)
    print(test_expression)
    #test_fails = [parameter_expression(t) for t in tests]
    embed()

if __name__ == '__main__':
    main()
