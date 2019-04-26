from pysercomb.utils import coln
from pysercomb.parsing import *
from pysercomb.parsers.racket import racket_doc, LEXEME


def get_quoted_list(folderpath, filename):
    with open((folderpath / filename).as_posix(), 'rt') as f:
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
                    lf, lr = len(f), len(r)
                    if lf > lr:
                        f, r = r, f
                    elif lf == lr:
                        fcase = lf == 3 and isinstance(f[0], str) and f[0].startswith('param:')
                        rcase = lr == 3 and isinstance(r[0], str) and r[0].startswith('param:')
                        if fcase and rcase:
                            if r[1] < f[1]:  # reorder commutative operations to be deterministic
                                f, r = r, f
                        elif rcase:  # fcase is the default so don't need to handle it
                            f, r = r, f

                if op in associative:  # assoc -> ok to move parens
                    if f[0] == op and r[0] == op:
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
    lisped = inner(return_value)
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


# basic tokens and operators
plus_or_minus_symbol = COMP('±')  # NOTE range and +- are interconvertable...
plus_or_minus_pair = COMP('+-')  # yes that is an b'\x2d'
plus_over_minus = COMP('+/-')
plus_or_minus = transform_value(OR(plus_or_minus_symbol, plus_or_minus_pair, plus_over_minus), lambda v: 'plus-or-minus')

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

def get_unit_dicts(units_path):
    return [get_quoted_list(units_path, _) for _ in
            ('si-prefixes-data.rkt',
             'si-prefixes-exp-data.rkt',
             'si-units-data.rkt',
             'si-units-extras.rkt',
             'units-dimensionless.rkt',
             'imperial-units-data.rkt')]
# units
def make_unit_parser(units_path):
    dicts = get_unit_dicts(units_path)
    gs = globals()
    for dict_ in dicts:
        gs.update(dict_)

    _silookup = {k: "'" + v
                 for k, v in units_si + units_extra +
                 tuple([v, v] for k, v in units_si) +
                 tuple([v, v] for k, v in units_extra)}
    _implookup = {k: "'" + v  # imperial separate because they don't support prefixes
                  for k, v in units_imp +
                  tuple([v, v] for k, v in units_imp)}
    _siplookup = {k: "'" + v for k, v in prefixes_si}

    siprefix = OR(*make_funcs(coln(0, prefixes_si), _siplookup))
    siunit = OR(*make_funcs(list(coln(0, units_si + units_extra)) + # need both here to avoid collisions in unit_atom slower but worth it?
                            list(coln(1, units_si + units_extra)), _silookup))
    impunit = OR(*make_funcs(list(coln(0, units_imp)) + list(coln(1, units_imp)), _implookup))

    DEGREES_UNDERLINE = b'\xc2\xba'.decode()  # º sometimes pdfs misencode these
    DEGREES_FEAR = b'\xe2\x97\xa6' # this thing is scary and I have no id what it is or why it wont change color ◦
    _C_for_temp = COMP('C')
    C_for_temp = RETVAL(_C_for_temp, BOX(_silookup['degrees-celcius']))
    temp_for_biology = JOINT(num, C_for_temp, join=False)

    unit_atom = param('unit')(BIND(OR(JOINT(siprefix, siunit, join=False),
                                      BIND(impunit, RETBOX),  # FIXME R RCF collision
                                      BIND(siunit, RETBOX)),  # merge just units?
                                   FLOP))

    maybe_exponent = AT_MOST_ONE(exponent)

    shorthand = BIND(BIND(JOINT(SKIP(COMPOSE(spaces, unit_atom),
                                     COMPOSE(spaces, maybe_exponent)),
                                int_),
                          lambda v: RETBOX(('^', *v))),
                     lambda v: RETURN(('*', *v)))
    unit_thing = OR(shorthand, unit_atom)
    unit_suffix = OR(JOINT(COMPOSE(spaces, unit_op),
                           COMPOSE(spaces, unit_thing)),
                     COMPOSE(spaces, shorthand))
    unit_expression = param('unit-expr')(BIND(BIND(JOINT(unit_thing,
                                                         BIND(MANY1(unit_suffix),
                                                              flatten1)),
                                                   flatten),
                                              op_order))
    unit = OR(unit_expression, unit_atom)
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
    suffix_unit_no_space = OR(param('unit')(OR(EXACTLY_ONE(fold_suffix), C_for_temp)), unit_starts_with_dash)  # FIXME this is really bad :/ and breaks dimensions...
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
                                                   BIND(quantity, RETBOX)))),
                                  flatten), RETBOX)
    expression = param('expr')(OR(prefix_expression, infix_expression))  # FIXME this doesn't work if you have prefix -> infix are there cases that can happen?

    boo = param('bool')(BIND(boolean, lambda v: RETBOX('#t') if v else RETBOX('#f')))

    #def approximate_thing(thing): return JOINT(EXACTLY_ONE(approx), COMPOSE(spaces, thing), join=False)

    def FAILURE(p):
        return param('parse-failure')(lambda null: (True, tuple(), p))(p)

    # TODO objective specifications...
    components = OR(dimensions,
                    dilution_factor,
                    expression,
                    quantity,
                    boo,
                    FAILURE)
    #approx_comp = approximate_thing(components)

    parameter_expression = components  # OR(approx_comp, components)

    debug_dict = {'infix_expression': infix_expression,
                  'prefix_expression': prefix_expression,
                 }

    return parameter_expression, quantity, unit, unit_atom, debug_dict


def main():
    import pprint
    from time import time
    from pathlib import Path
    from desc.prof import profile_me
    from IPython import embed
    from pysercomb.pyr.units import ProtcParameter

    units_path = Path('~/git/protc/protc-lib/protc/units').expanduser()
    parameter_expression, quantity, unit, unit_atom, debug_dict = make_unit_parser(units_path)

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
              '12-V', '10-mL', '+1',
             )
    should_fail = ('~~~~1',
                   "(pH 7.3",
                  )

    with open(Path('~/ni/dev/protocols/rkt/test-params.rkt').expanduser().as_posix(), 'rt') as f:
        success, v, rest = racket_doc(f.read())#[l.strip().strip('"') for l in f.readlines()][3:-1]
    param_test_strings = [s for e in v
                          if e[0] == 'define' and e[1] == 'param-test-strings'
                          for s in e[2]]
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
                pid.append(ProtcParameter(v))

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
    test_expression2 = '\n'.join(sorted((f"'{t+q:<25} -> {parameter_expression(t)[1]}" for t in tests + weirds), key=lambda v: v[25:]))
    print(test_expression2)
    test_fails = [parameter_expression(t) for t in tests]
    embed()


if __name__ == '__main__':
    main()
