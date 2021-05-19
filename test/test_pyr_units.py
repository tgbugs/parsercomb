import copy
import pickle
import pprint
import unittest
from decimal import Decimal
import pytest
import rdflib
from pysercomb import exceptions as exc
from pysercomb.parsers.units import DEGREES_FEAR
from pysercomb.pyr.units import ParamParser, SExpr, Expr
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


class TestUnitsParser(unittest.TestCase):

    def test_simple(self):
        ten_mega_liters = pyru.UnitsParser('10ML')
        assert isinstance(ten_mega_liters, SExpr)
        ap = ten_mega_liters.asPython()
        assert ap == pyru.ur.Quantity(10, 'ML')


class TestQuantity(unittest.TestCase):

    def test_add(self):
        q2 = pyru.ur.Quantity(1) + pyru.ur.Quantity(2)
        assert q2 == pyru.ur.Quantity(3)


class TestComparison(unittest.TestCase):
    q1 = pyru.ur.Quantity(1)
    Q2 = pyru.ur.Quantity(2)
    a1 = pyru.Approximately(q1)
    A2 = pyru.Approximately(Q2)
    f1 = pyru.ur.Quantity(1.1)
    F2 = pyru.ur.Quantity(1.2)
    d1 = pyru.ur.Quantity(Decimal(1.1))
    D2 = pyru.ur.Quantity(Decimal(1.2))
    t1 = q1
    T2 = Q2
    def test_eq(self):
        assert self.t1 == self.t1
        assert self.T2 == self.T2

    def test_gt(self):
        assert     self.T2 > self.t1
        assert not self.t1 > self.T2

    def test_ge(self):
        assert     self.t1 >= self.t1
        assert     self.T2 >= self.t1
        assert not self.t1 >= self.T2

    def test_lt(self):
        assert     self.t1 < self.T2
        assert not self.T2 < self.t1

    def test_le(self):
        assert     self.T2 <= self.T2
        assert     self.t1 <= self.T2
        assert not self.T2 <= self.t1


class TestF(TestComparison):
    t1 = TestComparison.f1
    T2 = TestComparison.F2


class TestD(TestComparison):
    t1 = TestComparison.d1
    T2 = TestComparison.D2


class TestComparisonUnits(TestComparison):
    q1 = pyru.ur.Quantity(1, 'mm')
    Q2 = pyru.ur.Quantity(2, 'mm')
    a1 = pyru.Approximately(q1)
    A2 = pyru.Approximately(Q2)


class TestApproximateComparison(TestComparison):
    t1 = TestComparison.a1
    T2 = TestComparison.A2

    def test_a(self):
        # approx are always lt AND gt when a1.q == q1
        assert     self.a1 < self.q1
        assert     self.a1 > self.q1
        assert     self.a1 != self.q1
        assert not self.a1 == self.q1

        # the only thing we know for sure is that they
        # are NOT equal which implies that we are modelling
        # approximately as an unknown probability distribution fuction
        # where P(X == x) = 0 ∀ x ∈ ℝ Rn ... the problem with unicode
        # is that you literally can't search for the darned things
        # thus latex ..

        # however, in cases where a1.q != Q
        # then the equality operators hold
        # for the following reason, we define
        # a1 < A2 ∀ a1, a2 where a1.q < A2.q
        # thus we must resolve the following
        # a1    ==    a1 < A2    ==    A2
        # a1 < a1.q < a1 < A2 < A2.q < A2
        #      a1.q      <      A2.q

        # all happy cases
        # a1   < a1.q < A2   < A2.q
        # a1.q < a1   < A2   < A2.q
        # a1   < a1.q < A2.q < A2
        # a1.q < a1   < A2.q < A2

        # when introducing the actual values (r)
        # we see how even assuming
        # a1 < A2 and a1.q < A2.q
        # can result in weirdness
        #        a1.r1 < A2.r1 < a1.q
        # A2.q < a1.r2 < A2.r2
        # further, a1.r and A2.r really have no specifed relation
        # A2.r < a1.r < a1.q < A2.q
        #               a1.q < A2.q < A2.r < a1.r
        # so the question is whether we treat the quantites as
        # the known parameters, or as the unknown actual values
        # for sake of sanity and sorting _for the visual representation_
        # we use the quantities and we lift the quantities to their
        # approximate form if they have to be compared with an
        # approximate value

        # this would seem to make intuitive sense given that
        # "approximately 20 oranges is less than approximate 22 oranges"
        # is going to be true more often than it is not
        # assuming that the approximating process is more or less the same
        # similarly "approximately 20 organges is less than 22 oranges"
        # is going to be a true statement more often than not as well
        # however the second statement might end up being true less frequently
        # than in the prior case because there is no opportunity for 22 oranges
        # to actually be 25 oranges when approx 20 oranges is actually 23 oranges
        # quiet the mess

        assert     self.a1 < self.Q2
        assert not self.a1 > self.Q2
        assert not self.a1 >= self.Q2

        assert     self.Q2 > self.a1
        assert not self.Q2 < self.a1
        assert not self.Q2 <= self.a1


class TestACF(unittest.TestCase):
    a1 = pyru.Approximately(TestComparison.f1)
    A2 = pyru.Approximately(TestComparison.F2)
    q1 = TestComparison.f1
    Q2 = TestComparison.F2
    test_a = TestApproximateComparison.test_a


class TestACD(unittest.TestCase):
    a1 = TestACF.a1
    A2 = TestACF.A2
    q1 = TestComparison.d1
    Q2 = TestComparison.D2
    test_a = TestApproximateComparison.test_a


class TAHelper:
    # pytest.mark actually marks functions, it doesn't just
    # decorate the local version, so have to create an
    # intermediate helper class that can hold the equality
    # and then be subclassed so we can call super ... sigh
    test_a = TestApproximateComparison.test_a


class TestApproximateComparisonUnits(TestComparisonUnits, TAHelper):
    # FIXME direct comparison with pint Quantity on the left has no
    # mediating function that we can use to catch and invert the the operation
    # so that approximately is on the left, this will need to be fixed
    # at some point in the future
    @pytest.mark.xfail
    def test_a(self):  # FIXME
        super().test_a()


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

    def test_problems(self):
        hz = pyru.UnitsParser('1 / s').asPython()

    def test_than_equal(self):
        g = pyru.UnitsParser('>= 20').asPython()
        l = pyru.UnitsParser('<= 20').asPython()

        ge = pyru.UnitsParser('≥ 20').asPython()
        le = pyru.UnitsParser('≤ 20').asPython()
        assert g == ge and l == le

    def test_iso8601(self):
        s = 'P1YT1S'
        oyos = pyru.UnitsParser(s).asPython()
        assert oyos._value == s, f'{oyos._value} != {s}'


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

    def test_measurement(self):
        t = pyru._Measurement(10, 1, 'days')
        self._doit(t)

    def test_parser_number(self):
        t = pyru.RacketParser('1')
        self._doit(t)

    def test_parser_sexp(self):
        t = pyru.RacketParser('"hello world"')
        self._doit(t)

    def test_parser_string(self):
        t = pyru.RacketParser('(+ 1 2)')
        self._doit(t)


class TestCopy(TestPickle):
    def _doit(self, thing):
        thing_prime = copy.deepcopy(thing)
        assert thing_prime == thing
