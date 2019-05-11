import pprint
import unittest
import rdflib
from pysercomb import exceptions as exc
from pysercomb.parsers.units import DEGREES_FEAR
from pysercomb.pyr.units import ParamParser, SExpr, Expr, UnitsParser, Quantity
from pysercomb.pyr import units as pyru
from .common import *

class TestPint(unittest.TestCase):
    def test_all(self):
        from pint import UnitRegistry
        bads = []
        for expr in test_all:
            try:
                out = UnitsParser(expr).asPython()
            except BaseException as e:
                bads.append((expr, e))

        assert not bads, '\n'.join(str(b) for b in bads)

