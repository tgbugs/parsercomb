import unittest
from .common import *
from pysercomb.parsers import units
from pysercomb.pyr import units as pyru


class TestForms(unittest.TestCase):
    def test_dilution(self):
        assert parameter_expression('1:1000')[1] == ('param:dilution', 1, 1000), 'dilution failed'

    def test_time_seconds(self):
        assert (parameter_expression('00:00:01')[1]
                == ('param:quantity', 1, ('param:unit', "'seconds"))), 'duration failed'

    def test_time_minutes(self):
        assert (parameter_expression('00:01:01')[1]
                == ('param:quantity', 61, ('param:unit', "'seconds"))), 'duration failed'

    def test_time_hours(self):
        assert (parameter_expression('01:01:01')[1]
                == ('param:quantity', 3661, ('param:unit', "'seconds"))), 'duration failed'

    def test_time_weird(self):
        assert (parameter_expression('1:99:01')[1]
                == ('param:quantity', 9541, ('param:unit', "'seconds"))), 'duration failed'


class TestUnit(unittest.TestCase):
    def test_minutes(self):
        """ make sure we don't parse min -> milli inches """
        assert unit('min') == (True, ('param:unit', "'minutes"), ''), 'min did not parse to minutes'

    def test_inches(self):
        assert unit('in') == (True, ('param:unit', "'inches"), ''), 'in did not parse to inches'

    def test_ohms(self):
        assert unit('R') == (True, ('param:unit', "'ohms"), ''), 'R did not parse to ohms'

    def test_rcf(self):
        assert unit('RCF') == (True, ('param:unit', "'relative-centrifugal-force"), ''), 'RCF did not parse to relative-centrifugal-force'

    def test_newtons(self):
        assert unit('N') == (True, ('param:unit', "'newtons"), ''), 'N did not parse to newtons'

    def test_numerical_aperture(self):
        msg = 'NA did not parse to numerical-aperture'
        assert unit('NA') == (True, ('param:unit', "'numerical-aperture"), ''), msg

    def test_fold(self):
        msg = 'Ax failed to parse as fold!'
        assert parameter_expression('40x')[1] == ('param:quantity', 40, ('param:unit', "'fold")), msg

    def test_dimension(self):
        msg = 'A x B failed to parse as dimensions!'
        assert parameter_expression('50 x 50 um')[1] == ('param:dimensions',
                                                         ('param:quantity', 50, ()),
                                                         ('param:quantity', 50,
                                                          ('param:unit', "'meters", "'micro"))), msg

    def test_percent(self):
        msg = '% failed to parse'
        assert parameter_expression('0.3%')[1] == ('param:quantity', 0.3, ('param:unit', "'percent")), msg

    def test_molar_space(self):
        out = pyru.UnitsParser(' 0.1M')
        assert out == ('param:quantity', 0.1, ('param:unit', "'molarity"))

    def test_parens(self):
        tests = (('s * m', ()),
                 ('s * mm', ()),
                 ('s * m^2', ()),
                 ('s * m ^ 2', ()),

                 ('s * m', ()),
                 ('s / m', ()),

                 ('mg kg–1', ()),
                 ('mg * kg–1', ()),
                 ('(J * m ) / s', ()),
                 ('(m * g) / s', ()),
                 ('s / m * g * l', ()),
                 ('M / (A * V * R)', ()),

                 ('as * m * g * l', ()),
                 ('fs / m * g / l', ()),

                 ('ps * m * (g * l)', ()),
                 ('ns / m * (g / l)', ()),

                 ('(s * m) * g * l', ()),
                 ('(s / m) * g / l', ()),

                 ('(s * m) * (g * l)', ()),
                 ('(s / m) * (g / l)', ()),
                 ('((s / m) * (g / l))', ()),

                 ('(m * g) / s^2', ()),
                 ('( m * g ) / s^2', ()),
                 #'(( m * g ) / s^2)',
                 ('(m * g) / s', ()),

                 ('(((m * g) * s) * J)', ()),
                 ('(m * (g * (s * J)))', ()),
        )
        ueatest = [unit_expr_atom(t) for t, e in tests]
        ueabad = [r for r in ueatest if r[-1]]
        uetest = [unit_expr(f'({t})') for t, e in tests]
        uebad = [r for r in uetest if r[-1]]
        ue1test = [unit_expr(f'{t}') for t, e in tests]
        ue1bad = [r for r in ue1test if r[-1]]
        uentest = [unit_expression(t) for t, e in tests]
        uenbad = [r for r in uentest if r[-1]]
        assert not uenbad


class TestExpr(unittest.TestCase):
    def test_parens(self):
        test = '(10 + 3) * 4'
        _, out, _ = parameter_expression(test)
        assert out == ('param:expr',
                       ('*',
                        ('param:quantity', 4, ()),
                        ('+', 
                         ('param:quantity', 10, ()),
                         ('param:quantity', 3, ()))))

    def test_mixed_unit_op_order_simple(self):
        test = '1 / mm^3'
        _, out, _ = parameter_expression(test)
        assert out == ('param:expr',
                       ('param:quantity', 1,
                        ('param:unit',
                           ('/', ('param:unit', "'count"),
                            ('^',
                             ('param:unit', "'meters", "'milli"),
                             ('param:quantity', 3, ()))))))

    def test_mixed_unit_op_order(self):
        test = '4.7 +- 0.6 x 10^7 / mm^3'
        test = '(4.7 +- 0.6 x 10^7) / mm^3'
        _, out, _ = parameter_expression(test)
        assert out == ('param:expr',
                       ('*',
                        ('plus-or-minus',
                         ('param:quantity', 4.7, ()),
                         ('param:quantity', 0.6, ())),
                        ('^',
                         ('param:quantity', 10, ()),
                         ('param:quantity',
                          7,
                          ('param:unit',
                           ('/', ('param:unit', "'count"),
                            ('^',
                             ('param:unit', "'meters", "'milli"),
                             ('param:quantity', 3, ()))))))))

    def test_prefix_infix_expr(self):
        text = '~1 - 3 mm'
        out = prefix_expression(text)
        test = (True,
                (('approximately',
                  ('range',
                   ('param:quantity', 1, ()),
                   ('param:quantity', 3,
                    ('param:unit', "'meters", "'milli")))),),
                '')
        assert out == test

    def test_prefix_expr(self):
        text = '>1'
        out = prefix_expression(text)
        test = (True,
                (('>', ('param:quantity', 1, ())),),
                '')
        assert out == test

    def test_infix_expr_many(self):
        text = '1 * 3 * 2 * 4'
        out = infix_expression(text)
        test = (True,
                (('*',
                  ('param:quantity', 1, ()),
                  ('param:quantity', 2, ()),
                  ('param:quantity', 3, ()),
                  ('param:quantity', 4, ())),),
                '')
        assert out == test

    def test_infix_expr_op_order_plus_mult(self):
        text = '1 * 2 + 4 * 3'
        out = infix_expression(text)
        test = (True,
                (('+',
                  ('*',
                   ('param:quantity', 1, ()),
                   ('param:quantity', 2, ())),
                  ('*',
                   ('param:quantity', 3, ()),
                   ('param:quantity', 4, ()))),),
                '')
        assert out == test

    def test_infix_expr_op_order_plus_mult_2(self):
        text = '2 * 1 + 4 * 5 * 3'
        out = infix_expression(text)
        test = (True,
                (('+',
                  ('*',
                   ('param:quantity', 1, ()),
                   ('param:quantity', 2, ())),
                  ('*',
                   ('param:quantity', 3, ()),
                   ('param:quantity', 4, ()),
                   ('param:quantity', 5, ()))),),
                '')
        assert out == test

    def test_infix_expr_noncommutative(self):
        """ do not reorder across non-commutative functions """
        text = '3 ^ 2'  # can't use subtraction since we don't parse it
        out = infix_expression(text)
        test = (True,
                (('^',
                  ('param:quantity', 3, ()),
                  ('param:quantity', 2, ())),),
                '')
        assert out == test


class TestMain(unittest.TestCase):
    def test_main(self):
        """ catch any changes here as well, eventually we should remove main for this ... """
        units.main()
