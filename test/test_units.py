import unittest
from .common import *
from pysercomb.parsers import units


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


class TestExpr(unittest.TestCase):
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
