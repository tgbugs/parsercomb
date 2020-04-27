import pickle
import pprint
import unittest
import rdflib
from pysercomb import exceptions as exc
from pysercomb.parsers.units import DEGREES_FEAR
from pysercomb.pyr.units import ParamParser, SExpr, Expr, UnitsParser, _Quant
from pysercomb.pyr import units as pyru
from .common import *

evil_white_dot = DEGREES_FEAR.decode()


class TestPint(unittest.TestCase):
    def _test_all(self):  # many known to fail due to numbers out front
        ur = pyru.ur
        bads = []
        for expr in test_all:
            try:
                out = ur.parse_expression(expr)
            except BaseException as e:
                bads.append((expr, e))

        assert not bads, bads


class TestParam(unittest.TestCase):
    def test_all(self):
        goods = []
        bads = []
        fails = []
        errs = []
        roundtrip = []
        paramparser = pyru.ParamParser()
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
        paramparser = pyru.ParamParser()
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
        assert ap == _Quant(10, 'ML')


class TestQuantity:
    def _test_add(self):
        q2 = Quantity(1) + Quantity(2)
        assert q2 == Quantity(3)


class TestUnits(unittest.TestCase):
    def test_export(self):
        a = pyru.UnitsParser('10 mm').asPython()
        aa = list(a.asRdf(rdflib.BNode()))
        b = pyru.UnitsParser('1 mg/kg').asPython()
        c = list(b.asRdf(rdflib.BNode()))
        d = pyru.UnitsParser('100 lm / 1000 W').asPython()
        e = list(d.asRdf(rdflib.BNode()))
        f = pyru.UnitsParser('10010.010110 g / 10mg').asPython()
        g = list(f.asRdf(rdflib.BNode()))
        r = pyru.UnitsParser('1-100T').asPython()
        s = list(r.asRdf(rdflib.BNode()))
        w = pyru.UnitsParser('9-14 weeks').asPython()
        x = list(w.asRdf(rdflib.BNode()))
        #breakpoint()

    def test_brokens(self):
        import ttlser  # workaround for entrypoints brokenness
        should_ser = pyru.UnitsParser('123 kHz / 1 J*K').asPython()
        should_ser.ttl

    def test_range_eq(self):
        r1 = pyru.Range(pyru._Quant('1 week'), pyru._Quant('10 weeks'))
        r2 = pyru.Range(pyru._Quant('1 week'), pyru._Quant('10 weeks'))
        assert r1 == r2


class TestPickle(unittest.TestCase):

    def _doit(self, thing):
        hrm = pickle.dumps(thing)
        tv = pickle.loads(hrm)
        assert tv == thing

    def test_quantity_unitful(self):
        t = pyru._Quant('10 days')
        self._doit(t)

    def test_quantity_unitless(self):
        t = pyru._Quant('10')
        self._doit(t)

    def test_range(self):
        t = pyru.Range(pyru._Quant('1 week'), pyru._Quant('10 weeks'))
        self._doit(t)

    def test_unit(self):
        t = pyru._Unit('month')
        self._doit(t)
