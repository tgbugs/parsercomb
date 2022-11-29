import pytest
import unittest
from .common import *
from pysercomb.parsers import units
from pysercomb.pyr import units as pyru


class TestForms(unittest.TestCase):

    def test_weird(self):  # TODO
        twice = parameter_expression('2x/day')

        eq = parameter_expression('mEq/mL')  # was just missing equivalents

        w0 = parameter_expression('10-to-12')
        w1 = parameter_expression('10 -to- 12')
        w2 = parameter_expression('10 - 12')
        w3 = parameter_expression('10 mg·kg−1·h−1')  # dot for multiplication
        w4 = parameter_expression('0.4V ~ 1.5V')  # infix approx

        w5 = parameter_expression('10 per day')
        w6 = parameter_expression('10 / day')
        w7 = parameter_expression('10per day')
        w8 = parameter_expression('10/ day')

        n0 = parameter_expression('1,2,3')  # not valid for commas
        n1 = parameter_expression('1000,000')  # not valid for commas
        n2 = parameter_expression('100,00')
        assert parameter_expression('1,000') == (True, ('param:quantity', 1000, ()), '')
        assert parameter_expression('100,000') == (True, ('param:quantity', 100000, ()), '')

    def test_ratio(self):
        assert parameter_expression('1:1000')[1] == ('param:ratio', 1, 1000), 'ratio failed'
        assert parameter_expression('2:1')[1] == ('param:ratio', 2, 1), 'ratio failed'

    def test_time_seconds(self):
        assert (parameter_expression('00:00:01')[1]
                == ('param:quantity', 1, ('param:unit', ('quote', 'seconds')))), 'duration failed'

    def test_time_minutes(self):
        assert (parameter_expression('00:01:01')[1]
                == ('param:quantity', 61, ('param:unit', ('quote', 'seconds')))), 'duration failed'

    def test_time_hours(self):
        assert (parameter_expression('01:01:01')[1]
                == ('param:quantity', 3661, ('param:unit', ('quote', 'seconds')))), 'duration failed'

    def test_time_weird(self):
        assert (parameter_expression('1:99:01')[1]
                == ('param:quantity', 9541, ('param:unit', ('quote', 'seconds')))), 'duration failed'

    def test_iso8601duration(self):
        # when you stop to think about it, it is kind of silly to report a
        # duration that crosses the months/days boundary due to the fact that
        # months have a variable number of days, so you will already be +/- a
        # day or 3 depending on the exact place in time where the duration is
        # specified, for use cases outside scientific I can image that it might
        # be useful
        #('P10D12H'),  # TODO should fail
        strings = (
            ('P1.5M', ('iso8601-duration-date',
                       ('param:quantity', 1.5, ('param:unit', ('quote', 'months'))),)),
            ('P1,5M', ('iso8601-duration-date',
                       ('param:quantity', 1.5, ('param:unit', ('quote', 'months'))),)),
            ('P1M', ('iso8601-duration-date',
                     ('param:quantity', 1, ('param:unit', ('quote', 'months'))),)),
            ('PT1M', ('iso8601-duration-time',
                      ('param:quantity', 1, ('param:unit', ('quote', 'minutes'))),)),
            ('P10D', ('iso8601-duration-date',
                      ('param:quantity', 10, ('param:unit', ('quote', 'days'))),)),
            ('PT12H', ('iso8601-duration-time',
                       ('param:quantity', 12, ('param:unit', ('quote', 'hours'))),)),
            ('P1Y1D', ('iso8601-duration-date',
                       ('param:quantity', 1, ('param:unit', ('quote', 'years'))),
                       ('param:quantity', 1, ('param:unit', ('quote', 'days'))),)),
            ('PT1M1S', ('iso8601-duration-time',
                        ('param:quantity', 1, ('param:unit', ('quote', 'minutes'))),
                        ('param:quantity', 1, ('param:unit', ('quote', 'seconds'))),)),
            ('P10DT12H', ('iso8601-duration-datetime',
                          ('param:quantity', 10, ('param:unit', ('quote', 'days'))),
                          ('param:quantity', 12, ('param:unit', ('quote', 'hours'))),)),
            ('P4Y3M10DT12H1M2S', ('iso8601-duration-datetime',
                                  ('param:quantity', 4, ('param:unit', ('quote', 'years'))),
                                  ('param:quantity', 3, ('param:unit', ('quote', 'months'))),
                                  ('param:quantity', 10, ('param:unit', ('quote', 'days'))),
                                  ('param:quantity', 12, ('param:unit', ('quote', 'hours'))),
                                  ('param:quantity', 1, ('param:unit', ('quote', 'minutes'))),
                                  ('param:quantity', 2, ('param:unit', ('quote', 'seconds'))),)),
        )
        bads = []
        for s, r in strings:
            ok, res, rest = iso8601duration(s)
            if r != res:
                print('exp:', r)
                print('got:', res)
                bads.append(s)

        assert not bads, bads

    def test_postnatal_day_vs_iso8601duration(self):
        assert (parameter_expression('P14')[1]
                # FIXME -day vs -days
                == ('param:quantity', 14,
                    ('param:prefix-unit', ('quote', 'postnatal-days')))), 'postnatal_day failed'
        assert (parameter_expression('P14D')[1]
                == ('iso8601-duration-date',
                    ('param:quantity', 14,
                     ('param:unit', ('quote', 'days'))))), 'iso8601duration failed'


class TestUnit(unittest.TestCase):

    def test_micron(self):
        assert unit('micron') == unit('um'), 'micron fail'

    def test_mmHg(self):
        expect = (True, ('param:unit', ('quote', 'millimeters-hg')), '')
        assert unit('mmHg') == expect, 'mmHgf'

    def test_kg_Kg(self):
        expect = (True, ('param:unit', ('quote', 'grams'), ('quote', 'kilo')), '')
        assert unit('kg') == expect, 'kgf'
        # if you can't get your SI prefixes correct I can't help you
        assert unit('Kg') == (True, ('param:unit', ('quote', 'kelvin')), 'g'), 'Kgf'

    def test_siprefix(self):
        assert debug_dict['siprefix']('milli') == (True, ('quote', 'milli'), ''), 'oops'
        assert debug_dict['siprefix']('deci') == (True, ('quote', 'deci'), ''), 'oops'

    def test_mm(self):
        expect = (True, ('param:unit', ('quote', 'meters'), ('quote', 'milli')), '')
        assert unit('mm') == expect, 'mmf'
        assert unit('mmeters') == expect, 'mmf'
        assert unit('millimeters') == expect, 'mmf'

    def test_becq(self):
        """ plural issues """
        expect = (True, ('param:unit', ('quote', 'becquerels')), '')
        assert unit('becquerels') == expect, 'becqf'
        assert unit('becquerel') == expect, 'becqf'
        assert unit('Bq') == expect, 'becqf'

    def test_minutes(self):
        """ make sure we don't parse min -> milli inches """
        expect = (True, ('param:unit', ('quote', 'minutes')), '')
        assert unit('minutes') == expect, 'minutes did not parse to minutes'
        assert unit('minute') == expect, 'minute did not parse to minutes'
        assert unit('min') == expect, 'min did not parse to minutes'

    def test_inches(self):
        assert unit('in') == (True, ('param:unit', ('quote', 'inches')), ''), 'in did not parse to inches'

    def test_ohms(self):
        assert unit('R') == (True, ('param:unit', ('quote', 'ohms')), ''), 'R did not parse to ohms'

    def test_rcf(self):
        assert unit('RCF') == (True, ('param:unit', ('quote', 'relative-centrifugal-force')), ''), 'RCF did not parse to relative-centrifugal-force'

    def test_newtons(self):
        assert unit('N') == (True, ('param:unit', ('quote', 'newtons')), ''), 'N did not parse to newtons'

    def test_numerical_aperture(self):
        msg = 'NA did not parse to numerical-aperture'
        assert unit('NA') == (True, ('param:unit', ('quote', 'numerical-aperture')), ''), msg

    def test_fold(self):
        msg = 'Ax failed to parse as fold!'
        assert parameter_expression('40x')[1] == ('param:quantity', 40, ('param:unit', ('quote', 'fold'))), msg

    def test_dimensions(self):
        msg = 'A x B failed to parse as dimensions!'
        # 50 x 50 um is ambiguous, unless additional information can be supplied
        # if you have the additional context then use dimensions_no_math
        assert parameter_expression('50 um x 50 um')[1] == ('param:dimensions',
                                                            ('param:quantity', 50,
                                                             ('param:unit', ('quote', 'meters'), ('quote', 'micro'))),
                                                            ('param:quantity', 50,
                                                             ('param:unit', ('quote', 'meters'), ('quote', 'micro')))), msg

    def test_dimensions_no_math(self):
        assert dimensions_no_math('50 x 50 um')[1] == ('param:dimensions',
                                                       ('param:quantity', 50, ()),
                                                       ('param:quantity', 50,
                                                        ('param:unit', ('quote', 'meters'), ('quote', 'micro')))), msg



    def test_percent(self):
        msg = '% failed to parse'
        assert parameter_expression('0.3%')[1] == ('param:quantity', 0.3, ('param:unit', ('quote', 'percent'))), msg

    def test_mm_range(self):
        expect = (True, ('param:quantity', ('param:expr', ('range', 5, 6)), ('param:unit', ('quote', 'meters'), ('quote', 'milli'))), '')
        assert parameter_expression('5-6mm') == expect, 'oops'
        assert parameter_expression('5-6 mm') == expect, 'oops'
        assert parameter_expression('5-6 millimeters') == expect, 'oops'

    def test_exp_vs_range(self):
        # range is fighting with exp_short
        # There is no good solution at this stage of the pipeline.
        # The issue is that a range separator without spaces can be parsed as a negative exponent.
        # Some authors like to put 1%-2% doubling the percent unit.
        # Percent does not combine with exp_short, which we can fix at the cost of some complexity,
        # but the underlying issue remains, e.g. with the 1kg-2kg example below.
        # One possible solution is to ensure that exp_short is not followed by another unit, which
        # is not viable because kg-1m is technically a valid unit, if horribly mangled I think.
        # Thus the best way forward is to check to see if there is a rest in the calling context
        # and if there is and there is a dash_thing in the input then should reparse with the
        # dash_thing surrounded by spaces, in which case the range will parse correctly and
        # there should be no rest value.
        o0 = unit('%-1')
        parsing._dobreak = True
        o1 = parameter_expression('1%-1')
        o5 = parameter_expression('1%-1%')

        k0 = parameter_expression('1kg-2kg')
        k1 = parameter_expression('1kg - 2kg')

        m0 = parameter_expression('1mm3-2mm3')
        m1 = parameter_expression('1mm3 - 2mm3')

        o2 = parameter_expression('1%+1')
        o3 = parameter_expression('1%*1')
        o4 = parameter_expression('1%/1')
        parsing._dobreak = False

    def test_molar_space(self):
        out = pyru.UnitsParser(' 0.1M')
        assert out == ('param:quantity', 0.1, ('param:unit', ('quote', 'molarity')))

    def test_unit_exp(self):
        res = exp_short('mm^3')
        _, out, _ = res
        assert out == ('^', ('param:unit', ('quote', 'meters'), ('quote', 'milli')), 3), res

        res = unit_thing('mm^3')
        _, out, _ = res
        assert out == ('^', ('param:unit', ('quote', 'meters'), ('quote', 'milli')), 3), res

        # should fail
        #res = unit_expr_atom('mm^3')
        #_, out, _ = res
        #assert out == ('^', ('param:unit', ('quote', 'meters'), ('quote', 'milli')), 3), res

        #res = unit('mm^3')

        res = unit_expression('mm^3')
        _, out, _ = res
        assert out == ('param:unit-expr', ('^', ('param:unit', ('quote', 'meters'), ('quote', 'milli')), 3)), res

        res = suffix_unit('mm^3')
        _, out, _ = res
        assert out == ('param:unit-expr', ('^', ('param:unit', ('quote', 'meters'), ('quote', 'milli')), 3)), res

    def test_implicit_count_ratio(self):
        ok, out, rest = suffix_unit(' / mm^3')
        assert out == ('param:unit-expr',
                       ('/', ('param:unit', ('quote', 'count')),
                        ('^', ('param:unit', ('quote', 'meters'), ('quote', 'milli')), 3))), (ok, out, rest)

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


class TestNumExpr(unittest.TestCase):

    func = staticmethod(num_expression)

    def test_0(self):
        res = self.func('0')
        _, out, _ = res
        assert out == 0, res

    def test_1(self):
        res = self.func('0 + 0')
        _, out, _ = res
        assert out == ('param:expr', ('+', 0, 0)), res

    def test_2(self):
        res = self.func('(2 + 1)')
        _, out, _ = res
        assert out == ('param:expr', ('+', 1, 2)), res

    def test_3(self):
        res = self.func('0 + 0 + 0')
        _, out, _ = res
        assert out == ('param:expr', ('+', 0, 0, 0)), res

    def test_3_0(self):
        res = self.func('(0 + 0) + 0')
        _, out, _ = res
        assert out == ('param:expr', ('+', 0, 0, 0)), res

    def test_3_1(self):
        res = self.func('0 + (0 + 0)')
        _, out, _ = res
        assert out == ('param:expr', ('+', 0, 0, 0)), res

    def test_4(self):
        res = self.func('(0 + 0 + 0)')
        _, out, _ = res
        assert out == ('param:expr', ('+', 0, 0, 0)), res

    def test_5(self):
        """ breaks the parser """
        res = self.func('((0 + 0) + 0)')
        _, out, _ = res
        assert out == ('param:expr', ('+', 0, 0, 0)), res

    def test_6(self):
        res = self.func('((0 + 0) + 0)')
        _, out, _ = res
        assert out == ('param:expr', ('+', 0, 0, 0)), res

    def test_7(self):
        res = self.func('((0 + 0) + 0)')
        _, out, _ = res
        assert out == ('param:expr', ('+', 0, 0, 0)), res

    def test_8(self):
        res = self.func('(25)')
        _, out, _ = res
        assert out == 25, res

    def test_8_0(self):
        res = self.func('-25')
        _, out, _ = res
        assert out == -25, res

    def test_9(self):
        res = self.func('(-25)')
        _, out, _ = res
        assert out == -25, res

    def test_num_expression(self):
        tests = (
            ('1', (True, 1, '')),
            ('1 + 2', (True, ('param:expr', ('+', 1, 2)), '')),
            ('(1 + 2)', (True, ('param:expr', ('+', 1, 2)), '')),
        )
        ntest = [(t, num_expression(t), e) for t, e in tests]
        nbad = [(t, r, e) for t, r, e in ntest if r != e]
        assert not nbad, '\n' + '\n'.join([f'{t}' for t in nbad])

    def test_parens(self):
        test = '(10 + 3) * 4'  # FIXME should be able to subsum all of this into a single expr
        _, out, _ = num_expression(test)
        assert out == ('param:expr', ('*', 4, ('+', 3, 10)))

    def test_infix_expr_many(self):
        text = '1 * 3 * 2 * 4'
        out = num_expression(text)
        test = (True,
                ('param:expr',
                 ('*',
                  1,
                  2,
                  3,
                  4),),
                '')
        assert out == test

    def test_infix_expr_op_order_plus_mult(self):
        text = '1 * 2 + 4 * 3'
        out = num_expression(text)
        test = (True,
                ('param:expr',
                 ('+',
                  ('*',
                   1,
                   2),
                  ('*',
                   3,
                   4)),),
                '')
        assert out == test

    def test_infix_expr_op_order_plus_mult_2(self):
        text = '2 * 1 + 4 * 5 * 3'
        out = num_expression(text)
        test = (True,
                ('param:expr',
                 ('+',
                  ('*',
                   1,
                   2),
                  ('*',
                   3,
                   4,
                   5)),),
                '')
        assert out == test

    def test_infix_expr_noncommutative(self):
        """ do not reorder across non-commutative functions """
        text = '3 ^ 2'  # can't use subtraction since we don't parse it
        out = num_expression(text)
        test = (True,
                ('param:expr', ('^', 3, 2),),
                '')
        assert out == test


class TestExpr(unittest.TestCase):
    def test_mixed_expr(self):
        tests = (
            ('(1 + 2) * (3 + 4)', ()),
            ('1 / mm^3', ()),
            ('(1 + 2) mm', ()),
            ('(3 + 4) mm^3', ()),
            ('(0 + 0) / mm^3', ()),
            ('(0 + (0 + 0))', ()),
            ('1 / 2 + 3 - 4', ()),
            ('1 / (2 + 3) - 4', ()),
            ('(1 / (2 + 3) - 4)', ()),
            ('(1 / (2 + 3) - 4) mm', ()),
            ('(1 / (2 + 3) - 4) mm^3', ()),
            ('(1 / (2 + 3) - 4) / mm^3', ()),
        )
        #qtest = [quantity(t) for t, e in tests]
        #qbad = [r for r in qtest if r[-1]]
        #assert not qbad, qbad
        #entest = [expression(t) for t, e in tests]
        #enbad = [r for r in entest if r[-1]]

        pentest = [parameter_expression(t) for t, e in tests]
        penbad = [r for r in pentest if r[-1]]
        assert not penbad, '\n' + '\n'.join([f'{t}' for t in penbad])

    def test_mixed_unit_op_order_simple(self):
        test = '1 / mm^3'
        _, out, _ = parameter_expression(test)
        print(out)
        assert out == ('param:quantity',
                       1,
                       ('param:unit-expr',
                        ('/', ('param:unit', ('quote', 'count')),
                         ('^', ('param:unit', ('quote', 'meters'), ('quote', 'milli')), 3))))

    @pytest.mark.skip('not fixed yet')
    def test_mixed_seconds(self):
        test = '1 / s'
        _, out, _ = parameter_expression(test)
        print(out)
        assert out == ('param:quantity',
                       1,
                       ('param:unit-expr',
                        ('/', ('quote', 'seconds'))))

    def test_mixed_unit_op_order(self):
        test = '4.7 +- 0.6 x 10^7 / mm^3'
        test = '(4.7 +- 0.6 x 10^7) / mm^3'
        _, out, _ = parameter_expression(test)
        print(out)
        assert out == ('param:quantity',
                       ('param:expr',
                        ('*',  # FIXME mark as param:expr?
                         ('plus-or-minus', 4.7, 0.6),
                         ('^', 10, 7))),
                       ('param:unit-expr',
                        ('/', ('param:unit', ('quote', 'count')),
                         ('^', ('param:unit', ('quote', 'meters'), ('quote', 'milli')), 3))))

    def test_prefix_infix_expr(self):
        text = '~1 - 3 mm'
        out = prefix_expression(text)
        test = (True,
                (('approximately',
                  ('param:quantity',
                   ('param:expr', ('range', 1, 3)),
                   ('param:unit', ('quote', 'meters'), ('quote', 'milli')))
                   ),),
                '')
        assert out == test

    def test_prefix_expr(self):
        text = '>1'
        out = prefix_expression(text)
        test = (True,
                (('>', ('param:quantity', 1, ())),),
                '')
        assert out == test

    def test_float_scinote(self):
        test = '0.000002 molarity'
        out = parameter_expression(test)
        assert out == (True,
                       ('param:quantity',
                        2e-06,
                        ('param:unit', ('quote', 'molarity'))),
                       '')


class TestMain(unittest.TestCase):
    def test_main(self):
        """ catch any changes here as well, eventually we should remove main for this ... """
        units.main()
