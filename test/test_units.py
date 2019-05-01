import unittest
from .common import *


class TestForms(unittest.TestCase):
    def test_time_vs_dilution(self):
        assert parameter_expression('1:1000') == (True, ('param:dilution', 1, 1000), ''), 'dilution failed'
        assert (parameter_expression('00:00:01')
                == (True, ('param:quantity', 1,
                           ('param:unit', "'seconds")), '')), 'duration failed'


class TestUnit(unittest.TestCase):
    def test_no_imperial_prefix(self):
        """ make sure we don't parse min -> milli inches """
        assert unit('min') == (True, ('param:unit', "'minutes"), ''), 'min did not parse to minutes'
        assert unit('in') == (True, ('param:unit', "'inches"), ''), 'in did not parse to inches'

    def test_rcf_ohms(self):
        assert unit('R') == (True, ('param:unit', "'ohms"), ''), 'R did not parse to ohms'
        assert unit('RCF') == (True, ('param:unit', "'relative-centrifugal-force"), ''), 'RCF did not parse to relative-centrifugal-force'


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
