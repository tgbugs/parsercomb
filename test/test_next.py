import pprint
import unittest
import rdflib
from pysercomb import exceptions as exc
from pysercomb.parsers.units import DEGREES_FEAR
from pysercomb.pyr.units import ParamParser, SExpr, Expr, UnitsParser, Quantity
from pysercomb.pyr import units as pyru
from .common import *

class TestPint(unittest.TestCase):
    def test_percent(self):
        from pysercomb.pyr.units import ur
        one = ur.parse_expression('10%')
        #print(one)
        two = ur.parse_units('percent')
        #print(two)
        #three = ur.parse_units('%')  # a bare percent conflicts with python mod
        #print(three)

    def test_all(self):
        from pint import UnitRegistry
        bads = []
        for expr in test_all:
            u = None
            try:
                u = UnitsParser(expr)
                out  = u.asPython()
            except exc.BadNotationError:
                pass
            except BaseException as e:
                raise e
                bads.append((expr, e, u))

        assert not bads, '\n'.join(str(b) for b in bads)

