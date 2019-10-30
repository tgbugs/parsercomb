import pprint
import unittest
import pytest
import rdflib
from pysercomb import exceptions as exc
from pysercomb.parsers.units import DEGREES_FEAR
from pysercomb.pyr.units import ParamParser, SExpr, Expr, UnitsParser
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

    @pytest.mark.skip('Expected to fail since the pint parser is simpler than ours.')
    def test_all(self):
        from pint import UnitRegistry
        bads = []
        fails = []
        for expr in test_all:
            u = None
            try:
                u = UnitsParser(expr)
                out  = u.asPython()
            except exc.BadNotationError:
                pass
            except exc.ParseFailure as e:
                fails.append((expr, e, u))
            except BaseException as e:
                raise e
                bads.append((expr, e, u))

        #assert not bads, '\n'.join(str(b) for b in bads)
        assert not fails, '\n'.join(str(f) for f in fails)

