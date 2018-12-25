import unittest
from pathlib import Path
from protcur.analysis import __script_folder__ as pasf
from pysercomb.parsers import units 

parameter_expression, *_, debug_dict = units.make_unit_parser(Path(pasf,
                                                                   '../../protc-lib/protc/units'))
# evil
_gs = globals()
_gs.update(debug_dict)


class TestUnits(unittest.TestCase):
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
