import numbers
from itertools import chain
from pysercomb import parsing
from pysercomb.utils import coln, log
from pysercomb.parsing import *
from pysercomb.parsers.racket import racket_doc, racket_module

totally_not_degrees = 'º◦˚⁰'  # come join the merry fools!
_superscript_zero = b'\xe2\x81\xb0'  # fails to read in a racket file on windows but not here ???


def LEXEME(func):
    return COMPOSE(whitespace, SKIP(func, whitespace))


def get_quoted_list(folderpath, filename):
    with open((folderpath / filename).as_posix(), 'rt') as f:
        src = f.read()
        success, value, rest = racket_doc(src)

    if not success:
        raise SyntaxError(f'Something is wrong in {filename}. Parse output:\n{value}\n\n{rest}')

    module = racket_module(value)
    out = {k.replace('-', '_'):v for k, v in module.items()}
    return out


def fix_plurals(ql):
    # XXX hack to deal with bad parsing of plurals
    # this solution can induce nasty performance issues
    # the correct solution is to build the parser so that
    # the plural form is optional and there are shared prefixes
    # this approach is quick and dirty so we don't worry about it

    def fix(u):
        if u == 'inches':
            return 'inch'
        else:
            return u[:-1]

    for k, v in ql.items():
        more = tuple([(fix(u), u) for p, u in v if u.endswith('s') and len(u) > 2 and not u.endswith('celsius')])
        ql[k] += more


def op_order(return_value):
    order = 'plus-or-minus', 'range', '^', '/', '*', '+', '-'
    associative = '*', '+'
    commutative = '*', '+'
    def key(subtree):
        if len(subtree) == 3 and isinstance(subtree[0], str) and subtree[0].startswith('param:'):
            return 0, subtree[1]  # short sort first
        else:
            return 1, len(subtree)

    def inner(subtree):
        for op in order[::-1]:
            try:
                i = subtree.index(op)
                front = subtree[:i]
                rest = subtree[i + 1:]
                f = inner(front)
                r = inner(rest)
                if op in commutative:
                    if isinstance(f, numbers.Number):
                        lf = 0
                    else:
                        lf = len(f)

                    if isinstance(r, numbers.Number):
                        lr = 0
                    else:
                        lr = len(r)

                    if lf > lr:
                        f, r = r, f
                    elif lf == lr != 0:
                        fcase = lf == 3 and isinstance(f[0], str) and f[0].startswith('param:')
                        rcase = lr == 3 and isinstance(r[0], str) and r[0].startswith('param:')
                        if fcase and rcase:
                            if isinstance(f[1], tuple):  # number expression
                                if isinstance(r[1], tuple):
                                    l1f, l1r = len(f[1]), len(r[1])
                                    if l1f > l1r:
                                        f, r = r, f  # put longer last
                                    elif l1f == l1r:
                                        pass   # TODO check relative values?
                                else:
                                    f, r = r, f  # put tuples last

                            elif isinstance(r[1], tuple):  # number expression
                                pass  # already ordered correctly with tuple last

                            elif r[1] < f[1]:  # reorder commutative operations to be deterministic
                                f, r = r, f

                        elif rcase:  # fcase is the default so don't need to handle it
                            f, r = r, f

                if op in associative:  # assoc -> ok to move parens
                    if isinstance(f, numbers.Number) and isinstance(r, numbers.Number):
                        if f < r:
                            val = f, r
                        else:
                            val = r, f

                        return (op, *val)

                    elif isinstance(f, numbers.Number):
                        if r[0] == op:
                            # this means that the operator on f is also op
                            # and so if op commutes then we can reorder
                            if op in commutative:
                                val = sorted((f, *r[1:]))
                            else:
                                val = f, *r[1:]

                        else:
                            val = f, r

                    elif isinstance(r, numbers.Number):
                        if f[0] == op:
                            if op in commutative:
                                val = sorted((f[1:], r))
                            else:
                                val = *f[1:], r
                        else:
                            val = r, f

                    elif f[0] == op and r[0] == op:
                        val = *f[1:], *r[1:]
                        if op in commutative:
                            val = sorted(val, key=key)

                    elif f[0] == op:
                        if op in commutative:
                            val = *sorted(f[1:], key=key), r
                        else:
                            val = *f[1:], r

                    elif r[0] == op:
                        # this means that the operator on f is also op
                        # and so if op commutes then we can reorder
                        # I think this works because it implies that f[0] is absent??!
                        if op in commutative:
                            val = sorted((f, *r[1:]), key=key)
                        else:
                            val = f, *r[1:]

                    else:
                        val = f, r
                else:
                    val = f, r

                return (op, *val)
            except ValueError:
                # can't index because we are at the bottom
                # FIXME this has ambiguous semantics
                continue

        return subtree[0]

    log.debug(return_value)
    lisped = inner(return_value)
    log.debug(lisped)
    if lisped != return_value:
        return RETURN((lisped,))
    else:
        return RETURN(return_value)


def param(prefix_name):
    name = 'param:' + prefix_name
    def add_function_type(v):
        return RETURN((name, *v))
    def paramed(parser_func):
        return BIND(parser_func, add_function_type)
    return paramed


debug = param('debug')

# basic tokens and operators
_plus_or_minus = '±'  # b'\xc2\xb1' '\u00b1'
plus_or_minus_symbol = COMP(_plus_or_minus)  # NOTE range and +- are interconvertable...
plus_or_minus_pair = COMP('+-')  # yes that is an b'\x2d'
plus_over_minus = COMP('+/-')
plus_or_minus = RETVAL(OR(plus_or_minus_symbol, plus_or_minus_pair, plus_over_minus), 'plus-or-minus')

addition = COMP('+')
subtraction = dash_thing
division = OR(COMP('/'), RETVAL(COMP('per'), '/'))  # also appears as x a/an time-unit, or x per something XXX TODO log normalizations
multiplication = RETVAL(OR(COMP('*'), by, parsing.dot), '*')  # ok as long as dimensions are specced with units 1 mm x 1 mm  # XXX FIXME by now includes X
# there is actually no way to tell the difference between
# 10 x 20 mm as 10 mm x 20 mm and 10 x 20 mm = 200 mm
math_op = OR(addition, subtraction, division, multiplication, exponent)  # FIXME subtraction is going to be a pain
unit_op = OR(division, multiplication)
lt = COMP('<')
_less_than_or_equal_to = '≤'  # b'\xe2\x89\xa4' '\u2264'
lte_symbol = COMP(_less_than_or_equal_to)
lte_pair = COMP('<=')
lte = RETVAL(OR(lte_pair, lte_symbol), '<=')
gt = COMP('>')
_greater_than_or_equal_to = '≥'  # b'\xe2\x89\xa5' '\u2265'
gte_symbol = COMP(_greater_than_or_equal_to)
gte_pair = COMP('>=')
gte = RETVAL(OR(gte_pair, gte_symbol), '>=')
comparison = OR(lte, gte, lt, gt)
approx = RETVAL(COMP('~'), 'approximately')

def get_unit_dicts(units_path):
    return [get_quoted_list(units_path, _) for _ in
            ('si-prefixes-data.rkt',
             'si-prefixes-exp-data.rkt',
             'si-units-data.rkt',
             'si-units-extras.rkt',
             'units-dimensionless.rkt',
             'imperial-units-data.rkt')]


DEGREES_UNDERLINE = b'\xc2\xba'.decode()  # º sometimes pdfs misencode these
DEGREES_FEAR = b'\xe2\x97\xa6'  # this thing is scary and I have no idea what it is or why it wont change color ◦
RING_ABOVE = b'\xcb\x9a'  # u02da apparently this thing can creep in as a degrees symbol sometimes as well ??!?

def hms(h, m, s):
    """ hours minutes seconds to seconds """
    return h * 60 * 60 + m * 60 + s

# units
def make_unit_parser(units_path=None, dicts=None):
    if units_path is None and dicts is None:
        raise TypeError('must have units_path or dicts')
    if dicts is None:
        dicts = get_unit_dicts(units_path)

    gs = globals()
    for dict_ in dicts:
        if [k for k in ['units_si', 'units_extra', 'units_imp'] if k in dict_]:
            fix_plurals(dict_)
        gs.update(dict_)

    _silookup = {k: ('quote', v)
                 for k, v in chain(units_si,
                                   units_extra,
                                   ([v, v] for k, v in units_si),
                                   ([v, v] for k, v in units_extra))}
    _implookup = {k: ('quote', v)  # imperial separate because they don't support prefixes
                  for k, v in chain(units_imp,
                                    units_dimensionless,
                                    ([v, v] for k, v in units_imp),
                                    ([v, v] for k, v in units_dimensionless))}
    _siplookup = {k: ('quote', v) for k, v in chain(prefixes_si,
                                                    ([v, v] for k, v in prefixes_si))}

    siprefix = OR(*make_funcs(chain(coln(0, prefixes_si),
                                    coln(1, prefixes_si)),
                              _siplookup))
    siunit = OR(*make_funcs(chain(coln(0, units_si + units_extra), # need both here to avoid collisions in unit_atom slower but worth it?
                                  coln(1, units_si + units_extra)),
                            _silookup))
    impunit = OR(*make_funcs(chain(coln(0, units_imp + units_dimensionless),
                                   coln(1, units_imp + units_dimensionless)),
                             _implookup))

    def parenthized(func): return COMPOSE(LEXEME(COMP('(')), SKIP(func, LEXEME(COMP(')'))))
    def parOR(func): return OR(func, parenthized(func))

    to = COMP('to')
    dtod = COMP('-to-')  # not matching ...
    range_indicator = RETVAL(OR(dtod, thing_accepted_as_a_dash, to), 'range')
    # FIXME range vs minus ...
    infix_operator = OR(plus_or_minus, range_indicator, math_op)  # colon? doesn't really operate on quantities, note that * and / do not interfere with the unit parsing because that takes precedence

    def num_expression(thing): return OR(param('expr')(parOR(_num_expression)), parOR(OR(num_commas, num)))(thing)
    def num_expression_inner(thing): return OR(parOR(_num_expression), OR(num_commas, num))(thing)

    _dig = BIND(parsing.TIMES(digit, 1, 3), lambda i: RETURN(''.join(i)))
    _3dig = BIND(parsing.TIMES(digit, 3, 3), lambda i: RETURN(''.join(i)))
    num_commas = BIND(JOINT(_dig, MANY1(COMPOSE(COMP(','), _3dig)), join=True),
                      lambda i: RETURN(int(''.join(i))))

    num_thing = OR(parOR(OR(num_commas, num)), BIND(parenthized(num_expression_inner), flatten1))
    num_suffix = JOINT(COMPOSE(spaces, infix_operator),
                       COMPOSE(spaces, num_thing))

    _num_expression = BIND(BIND(JOINT(num_thing,
                                      BIND(MANY1(num_suffix),
                                           flatten1)),
                                flatten),
                           op_order)

    # biology specific extensions
    _C_for_temp = COMP('C')
    C_for_temp = RETVAL(_C_for_temp, BOX(_silookup['degrees-celsius']))
    temp_for_biology = JOINT(num, C_for_temp, join=False)  # XXX not used
    mmHg = BIND(OR(COMP('mmHg'), COMP('mm Hg')), (lambda _: RETURN((('quote', 'millimeters-hg'),))))  # FIXME move to data files
    RCF = BIND(OR(COMP('RCF'), COMP('xg')), (lambda _: RETURN((_implookup['RCF'],))))  # since ronna R broke all the things  XXX xg lacks priority due to fold being in suffix unit no space
    dpi = BIND(COMP('dpi'), (lambda _: RETURN((('quote', 'pixels-per-inch'),))))  # usually uppercase
    micron = BIND(COMP('micron'), (lambda _: RETURN((('quote', 'micro'), ('quote', 'meters')))))
    def sigh(p): return parsing.oper(p, lambda s: s in totally_not_degrees)
    degc_mess = BIND(OR(COMP('degrees Celsius'),
                        COMP('degrees C'),
                        COMP('o celcius'),
                        COMP('oC'),
                        JOINT(sigh, _C_for_temp)),
                     (lambda _: RETURN((_silookup['~oC'],))))

    manual_unit = OR(mmHg, RCF, degc_mess, micron, dpi)
    unit_atom = param('unit')(BIND(OR(manual_unit,
                                      JOINT(siprefix, siunit, join=False),
                                      BIND(impunit, RETBOX),  # FIXME R RCF collision
                                      BIND(siunit, RETBOX)),  # merge just units?
                                   FLOP))

    maybe_exponent = LEXEME(AT_MOST_ONE(exponent))  # XXX must be AT_MOST_ONE, but issues with ranges, see test_exp_vs_range

    exp_short = BIND(JOINT(SKIP(COMPOSE(spaces, unit_atom),  # XXX TODO FIXME need unit_atom_not_dimensionless
                                COMPOSE(spaces, maybe_exponent)),
                           int_),
                     lambda v: RETURN(('^', *v)))
    exp_no_op = (BIND(exp_short, lambda v: RETURN(('*', v))))  # FIXME no preceeded by an operator ...
    unit_thing = OR(exp_short, unit_atom)
    def unit_expr(thing): return parOR(unit_expr_atom)(thing)
    def unit_par(thing): return parenthized(unit_expr_atom)(thing)
    #unit_implicit_count_ratio = BIND(LEXEME(division), lambda v: RETBOX((v[0], unit("count")[1], *v[1:])))
    def UICR(func):
        return BIND(JOINT(LEXEME(division), func),
                    lambda v: RETBOX((v[0], unit("count")[1], *v[1:])))
    #unit_op_prefix = unit_implicit_count_ratio
    unit_suffix = OR(JOINT(COMPOSE(spaces, unit_op),
                           COMPOSE(spaces, OR(unit_thing, BIND(unit_expr, flatten1)))),
                     COMPOSE(spaces, exp_no_op))
    # I think the issue is cases like 1 + (2 * 3) - 4 ...
    unit_expr_atom = BIND(BIND(JOINT(OR(UICR(unit_thing), #JOINT(unit_op_prefix, unit_thing),
                                        unit_thing,
                                        BIND(unit_par, flatten1)),
                                     BIND(MANY1(unit_suffix),
                                          flatten1)),
                               flatten),
                          op_order)

    unit_expression = param('unit-expr')(OR(unit_expr,
                                            BIND(exp_short, RETBOX),
                                            UICR(unit_expr),
                                            UICR(exp_short)))

    unit = OR(unit_expression, unit_atom)

    unit_starts_with_dash = COMPOSE(dash_thing, unit)  # derp
    #unit_implicit_count_ratio = param('unit-expr')(_unit_implicit_count_ratio)

    def plus_or_minus_thing(thing): return JOINT(plus_or_minus, COMPOSE(spaces, thing), join=False)

    def range_thing(func): return JOINT(func, COMPOSE(spaces, range_indicator), COMPOSE(spaces, func))

    # XXX FIXME loss of representation issues are possible here
    # the solution is to represent the value as a sum I think
    _dur_digits = BIND(MANY1(digit), lambda v: RETURN(''.join(v)))
    _dur_number = OR(BIND(JOINT(_dur_digits, RETVAL(OR(comma, point), '.'), _dur_digits, join=True),
                          # FIXME precision etc.
                          lambda v: RETURN(float(''.join(v)))),
                     # FIXME -> represent
                     BIND(_dur_digits, lambda v: RETURN(int(v))))
    _, _dur_date, _, _dur_time = (
        (_dur_P,),
        (_dur_Y, _dur_M, _dur_D),
        (_dur_T,),
        (_dur_H, _dur_M, _dur_S)
    ) = [[RETVAL(COMP(l), val) for l, val in zip(ls, vals)]
         for ls, vals in zip(('P', 'YMD', 'T', 'HMS'),
                       ((None,),
                        [('quote', u) for u in ('years', 'months', 'days')],
                        (None,),
                        [('quote', u) for u in ('hours', 'minutes', 'seconds')]))]
    # technically these could be parsed in any order because we know the type of
    # the individual duration, however there are some weird impliciations about
    # the ordering, depending on which section you count first
    #  012   01   02   12   0   1   2
    # 'YMD' 'YM' 'YD' 'MD' 'Y' 'M' 'D'
    # 'HMS' 'HM' 'HS' 'MS' 'H' 'M' 'S'
    _inds = [[0, 1, 2], [0, 1], [0, 2], [1 ,2], [0], [1], [2]]
    combs = _c_date, _c_time = [[[param('quantity')
                                  (ANDTHEN(_dur_number,
                                           param('unit')
                                           (BIND(elems[index], RETBOX))))
                                  for index in indexes]
                                 for indexes in _inds]
                                for elems in (_dur_date, _dur_time)]
    _p_date, _p_time = [OR(*[JOINT(*parts, join=False)
                             for parts in comb])
                        for comb in combs]
    _tp_time = COMPOSE(_dur_T, _p_time)
    _p_datetime = JOINT(_p_date, _tp_time, join=True)
    iso8601duration = COMPOSE(
        _dur_P,
        # these sort into different functions because durations that
        # include dates are invariants while time alone is a parameter
        # this helps with equality checking among other things
        OR(BIND(_tp_time,
                lambda v: RETURN(('iso8601-duration-time', *v))),
           BIND(_p_datetime,
                lambda v: RETURN(('iso8601-duration-datetime', *v))),
           BIND(_p_date,
                lambda v: RETURN(('iso8601-duration-date', *v))),))

    pH = RETVAL(COMP('pH'), BOX("'pH"))
    post_natal_day = RETVAL(COMP('P'), BOX(('quote', 'postnatal-days')))  # FIXME note that in our unit hierarchy this is a subclass of days
    _fold_prefix = END(by, num)
    fold_prefix = RETVAL(_fold_prefix, BOX(('quote', 'fold')))

    prefix_unit = param('prefix-unit')(OR(pH, post_natal_day, fold_prefix))
    _prefix_quantity = JOINT(prefix_unit, COMPOSE(spaces, num_expression))  # OR(JOINT(fold, num))
    prefix_quantity = BIND(_prefix_quantity, FLOP)

    # num only prefix quantities
    _prefix_quantity_num_only = JOINT(prefix_unit, COMPOSE(spaces, num))  # OR(JOINT(fold, num))
    prefix_quantity_num_only = BIND(_prefix_quantity, FLOP)

    fold_suffix = RETVAL(END(by, noneof('0123456789')), BOX(('quote', 'fold')))  # NOT(num) required to prevent issue with dimensions
    suffix_unit = unit #OR(unit_implicit_count_ratio, unit)
    suffix_unit_no_space = OR(param('unit')(OR(EXACTLY_ONE(fold_suffix), C_for_temp)), unit_starts_with_dash)  # FIXME this is really bad :/ and breaks dimensions...
    suffix_quantity = JOINT(num_expression,
                            OR(suffix_unit_no_space,
                               COMPOSE(spaces,
                                       AT_MOST_ONE(suffix_unit, fail=False))))  # this catches the num by itself and leaves a blank unit
    def infix_par(thing): return parenthized(infix_expression)(thing)
    quantity = param('quantity')(OR(prefix_quantity, suffix_quantity))

    # quantity expressions that require a trailing unit
    suffix_quantity_with_unit = JOINT(num_expression,
                                      OR(suffix_unit_no_space,
                                         COMPOSE(spaces,
                                                 suffix_unit)))
    quantity_with_unit = param('quantity')(OR(prefix_quantity, suffix_quantity_with_unit))

    # quantity expressions that require no numerical expressions, only numbers
    suffix_quantity_num_only = JOINT(OR(num_commas, num),
                                     OR(suffix_unit_no_space,
                                        COMPOSE(spaces,
                                                AT_MOST_ONE(suffix_unit, fail=False))))
    quantity_num_only = param('quantity')(OR(prefix_quantity_num_only, suffix_quantity_num_only))

    _hmsret = lambda v: param('quantity')(JOINT(RETURN(hms(*v)),
                                                param('unit')(RETURN((('quote', 'seconds'),)))))
    duration_hms = BIND(JOINT(SKIP(int_, colon), SKIP(int_, colon), int_, join=False), _hmsret)  # FIXME loss of repr
    #duration_hm = BIND(JOINT(SKIP(int_, colon), int_, join=False), _hmsret)  # TODO FIXME loss of repr
    ratio = param('ratio')(JOINT(SKIP(int_, colon), int_, join=False))  # dilution starts with 1 but is an aspect
    sq = COMPOSE(spaces, quantity)
    sby = COMPOSE(spaces, by)
    dimensions = param('dimensions')(BIND(JOINT(quantity_with_unit,
                                                MANY1(COMPOSE(COMPOSE(spaces, SKIP(by, spaces)),
                                                              END(quantity, NOT(exponent))))),  # catch A x B^C
                                          flatten))

    # use this in situations where A x B implies dimensions rather than multiplication
    # this is no longer the default assumption for the parser
    dimensions_no_math = param('dimensions')(BIND(JOINT(quantity_num_only,
                                                        MANY1(COMPOSE(COMPOSE(spaces, SKIP(by, spaces)),
                                                                      END(quantity, NOT(exponent))))),
                                                  flatten))
    prefix_operator = OR(approx, plus_or_minus, comparison)  # FIXME approx is not really an operator
    # TODO when parsing approx actually has higher affinity for the
    # number than the units, because it modifies the certainty about
    # the magnitude independent of the units and thus should probably
    # parse as (quantity (~ 10)) instead of (~ (quantity 10))
    def infix_expr(thing): return parOR(infix_expression)(thing)
    infix_suffix = JOINT(COMPOSE(spaces, infix_operator),
                         COMPOSE(spaces, OR(quantity, BIND(infix_expr, flatten1))))
    infix_expression = BIND(BIND(JOINT(OR(quantity, BIND(infix_par, flatten1)),
                                       BIND(MANY1(infix_suffix),
                                            flatten1)),
                                 flatten),
                            op_order)
    prefix_expression = BIND(BIND(JOINT(prefix_operator,
                                        COMPOSE(spaces,
                                                OR(infix_expression,
                                                   BIND(quantity, RETBOX)))),
                                  flatten), RETBOX)
    expression = param('expr')(OR(prefix_expression, infix_expression))  # FIXME this doesn't work if you have prefix -> infix are there cases that can happen?

    boo = param('bool')(BIND(boolean, lambda v: RETBOX('#t') if v else RETBOX('#f')))

    #def approximate_thing(thing): return JOINT(EXACTLY_ONE(approx), COMPOSE(spaces, thing), join=False)

    def FAILURE(p):
        # NOTE we do not return (True, (p,), p) because FAILURE
        # should only be used in the last slot, nothing should be
        # parsing after this in the same chain
        return param('parse-failure')(lambda null: (True, (p,), ''))(p)

    # TODO objective specifications...
    components = OR(dimensions,
                    duration_hms,  # must come first otherwise ratio will always match first
                    ratio,
                    iso8601duration,  # FIXME it should be possible to compose this
                    expression,
                    quantity,
                    boo,
                    FAILURE)
    #approx_comp = approximate_thing(components)


    parameter_expression = LEXEME(components)  # OR(approx_comp, components)

    debug_dict = {'infix_expression': infix_expression,
                  'prefix_expression': prefix_expression,
                  'unit_expr_atom': unit_expr_atom,
                  'unit_expr': unit_expr,
                  'unit_expression': unit_expression,
                  'num_expression': num_expression,
                  'suffix_unit': suffix_unit,
                  'exp_short': exp_short,
                  'unit_thing': unit_thing,
                  'expression': expression,
                  'num_thing': num_thing,
                  'dimensions_no_math': dimensions_no_math,  # TODO
                  'iso8601duration': iso8601duration,
                  'siprefix': siprefix,
                 }

    return parameter_expression, quantity, unit, unit_atom, debug_dict


def parse_for_tests(parameter_expression=None):

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
             '10lbs', '11 lbs',
             'TRUE', 'fAlsE', '#t', '#f',
             '10 \u00B5L', '10 \u03BCL',  # micro issues
             '1 s-1', '1 h-1', '1 mg kg-1 h-1',
            )

    prefix_expr_tests = ('<1', '~3.5 - 6 Mohms',)

    weirds = ("One to 5", "100-Hz", "25 ng/ul)", "34–36°C.",
              '3*10^6 infectious particles/mL',
              '4.7 +- 0.6 x 10^7 / mm^3',  # FIXME this is ambigious? YES VERY also unit dimensionality...
              '1,850', '4C', 'three', 'Four', 'P28.5±2 days',
              '12-V', '10-mL', '+1', 'Forty seconds', '10s - 100h'
             )
    should_fail = ('~~~~1',
                   "(pH 7.3",
                  )

    from pathlib import Path
    from protcur.config import __units_test_params__ as test_params
    with open(test_params, 'rt') as f:
        success, value, rest = racket_doc(f.read())

    module = racket_module(value)
    param_test_strings = module['param-test-strings']

    _all = tests + prefix_expr_tests + weirds + should_fail + param_test_strings
    if parameter_expression:
        from pysercomb.pyr import units as pyru
        up = pyru.UnitsParser
        parsed = [up(t) for t in _all]
    else:
        parsed = None

    return tests, prefix_expr_tests, weirds, should_fail, param_test_strings, _all, parsed


def main():
    import pprint
    from time import time
    from pathlib import Path
    from pysercomb.pyr.units import SExpr
    from protcur.config import __units_folder__ as units_path

    try:
        from desc.prof import profile_me
    except ImportError:
        profile_me = lambda f: f

    (parameter_expression, quantity, unit,
     unit_atom, debug_dict) = make_unit_parser(units_path)

    (tests, prefix_expr_tests, weirds, should_fail, param_test_strings, _all,
     parsed) = parse_for_tests()

    test_all = []

    pid = []
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
            else:
                pid.append(SExpr(v))

            test_all.append((success, v, rest))
    start = time()
    timeit()
    stop = time()
    print('BAD TIME', stop - start)
    print_issues = {i:h for i, h in enumerate(pid)} 
    # pprint.pprint(print_issues)  # this works correctly

    q = "'"
    fun = [t.split(' ')[-1] for t in tests][:5]
    test_unit_atom = [unit_atom(f) for f in fun]
    test_unit = [unit(f) for f in fun]
    test_quantity = [quantity(t) for t in tests]
    test_expression = [parameter_expression(t) for t in tests + prefix_expr_tests + weirds]
    test_expression2 = '\n'.join(sorted((f"'{t+q:<25} -> {parameter_expression(t)[1]}"
                                         for t in tests + weirds), key=lambda v: v[25:]))
    print(test_expression2)
    test_fails = [parameter_expression(t) for t in tests]
    if __name__ == '__main__':
        from IPython import embed
        embed()


if __name__ == '__main__':
    main()
