import pprint
import unittest
from pysercomb import exceptions as exc
from pysercomb.parsers.units import DEGREES_FEAR
from pysercomb.pyr.units import ParamParser, SExpr, Expr, UnitsParser, Quantity
from .common import *

evil_white_dot = DEGREES_FEAR.decode()

class TestParam(unittest.TestCase):
    def test_all(self):
        goods = []
        bads = []
        fails = []
        errs = []
        roundtrip = []
        paramparser = ParamParser()
        for text, ir in zip(test_all, parsed):
            try:
                unit = paramparser(ir)
                if unit is not None:  # False is a valid result
                    new_text = str(unit)
                    new_ir = parameter_expression(text)
                    roundtrip.append((ir, new_ir))
                    debug = ('', SExpr(ir), ir, text, new_text)
                    if evil_white_dot in new_text:
                        bads.append(debug)
                    else:
                        goods.append(debug)
                else:
                    raise ValueError(f'how? {unit} {text} {SExpr(ir)}')
            except exc.BadNotationError as e:
                bads.append((repr(e), SExpr(ir), ir, text, ''))
            except exc.ParseFailure as e:
                fails.append((repr(e), SExpr(ir), ir, text, ''))
            except BaseException as e:
                errs.append((repr(e), SExpr(ir), ir, text, ''))
                raise e

        #pprint.pprint([g for g in goods])
        assert not errs, '\n'.join([f'{e}\n{sexpr}\n{ir}\n{text}\n{new_text}'
                                    for e, sexpr, ir, text, new_text in errs])

    def test_roundtrip(self):
        # mostly seeing order inversion issues and unit vs unit-expression
        roundtrip = []
        paramparser = ParamParser()
        for text, ir in zip(test_all, parsed):
            try:
                unit = paramparser(ir)
                if unit is not None:
                    new_text = str(unit)  # hrm?
                    _, new_ir, _ = parameter_expression(text)
                    roundtrip.append((text, new_text, ir, new_ir))
                else:
                    raise ValueError(f'how? {unit} {text} {SExpr(ir)}')

            except exc.BadNotationError as e:
                pass  # we deal with these above
            except exc.ParseFailure as e:
                pass  # we deal with these above
            except BaseException as e:
                raise e

        bads = [(text, new_text, SExpr(a), SExpr(b)) for text, new_text, a, b in roundtrip
                if a != b]

        join = '\n===============================================\n'
        assert not bads, '\n' + join.join([f'{text!r}\n{new_text!r}\n{pa}\n{pb}'
                                           for text, new_text, pa, pb in bads])

class TestUnitsParser:
    def test_simple(self):
        ten_mega_liters = UnitsParser('10ML')
        assert isinstance(ten_mega_liters, SExpr)
        ap = ten_mega_liters.asPython()
        assert ap == Quantity(10, 'ML')
