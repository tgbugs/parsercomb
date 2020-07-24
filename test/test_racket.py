import unittest
import pytest
from pysercomb.parsers import racket
from pysercomb.parsing import float_, int_, scientific_notation
from pysercomb.pyr import units as pyru


class TestForms(unittest.TestCase):
    def test_int(self):
        res = int_('1')
        _, v, _  = res
        assert v == 1, res

    def test_int_leading_zero(self):
        res = int_('00001')
        _, v, _  = res
        assert v == 1, res

        res = int_('-00001')
        _, v, _  = res
        assert v == -1, res

    def test_scinot(self):
        _, v, _ = scientific_notation('1E10')
        assert v == 1E10, v
        assert repr(v) == '1E10', res

    def test_scinot_leading_zero(self):
        # REMINDER: the primary use case for this format
        # is as a means to record the exact notation used
        # by an investigator, therefore we can't parse this
        # to a float, HOWEVER this induces a roundtripping
        # issue when a float that is _not_ provided in
        # scientific notation is converted to scientific notation
        res = _, v, _ = scientific_notation('2e-06')
        assert v == 2e-06, res
        assert repr(v) == '2e-06', res

    def test_scinot_neg(self):
        res = scientific_notation('1')
        _, v, rest = res
        assert v is None and rest == '1', (v, rest)

    def test_quote(self):
        res = racket.racket_doc("'(a (1 . 2))")
        print(res)
        _, (v,), _ = res
        assert v == ('quote', ('a', (1, 2)))

    def test_cons_literal(self):
        _, (v,), _ = racket.racket_doc("'(1 . 2)")
        assert v == ('quote', (1 , 2))

    def test_null_t_f(self):
        _, (v,), _ = racket.racket_doc("'(null #t #f)")
        assert v == ('quote', ('null', True, False))

    def test_empty_list(self):
        _, (v,), _ = racket.racket_doc("'()")
        assert v is None

    def test_number(self):
        res = racket.racket_doc('1')
        _, (v,), _ = res 
        assert v == 1, res

    def test_null(self):
        code = """#lang racket/base
(provide something)
(define something
'((a . b)
  (() . 0)
  (c . 1)))
"""
        res = racket.racket_doc(code)
        print(res)
        _, v, _ = res
        assert v == ('module-unexp', None, ('racket/base',),
                     ('module-begin',
                      ('provide', 'something'),
                      ('define', 'something',
                       ('quote', (('a', 'b'),
                                  (tuple(), 0),
                                  ('c', 1))))))

    def test_string_escape(self):
        ok, value, rest = racket._string_escape(r'\"')
        assert ok

    def test_string_escape_full(self):
        code = r'''"testing \"string\" escape"'''
        res = racket.exp(code)
        print(res)
        _, v, _ = res
        assert v == 'testing "string" escape'

    def test_quote_sexp(self):
        code = """'(TEAPOTS ON THE OH NO)"""
        res = racket.exp(code)
        print(res)
        _, v, _ = res
        assert v == ('quote', ('TEAPOTS', 'ON', 'THE', 'OH', 'NO'))

    def test_quote_has_number(self):
        code = """'a4sdf"""
        res = racket.exp(code)
        print(res)
        _, v, _ = res
        assert v == ('quote', 'a4sdf')

    def test_quote_start_number(self):
        code = """'4asdf"""
        res = racket.exp(code)
        print(res)
        _, v, _ = res
        assert v == ('quote', '4asdf')

    def test_number_literal_fail(self):
        code = """4asdf"""
        ok, v, rest = res = racket.literal(code)
        print(res)
        ok, v, _ = res
        assert not ok

    def test_number_literal_ends(self):
        codes = [
            """4(+ 1 2)""",
            """4;lol""",
            """4)""",
            """4'hello""",
            """4""",
            '''4"there"'''
        ]
        for code in codes:
            ok, v, rest = res = racket.literal(code)
            print(res)
            assert ok and v == 4 and (code == '4' or rest)

    def test_multi_comment_exp(self):
        code = '''; c1
; c2
; how many HAHA! third times the charm
(+ 1  ; asdf
   #:k-w 3 ;oh no
   2) ; asdf
; c3
; c4'''
        ok, v, rest = res = racket.exp(code)
        assert ok and v == ('+', 1, '#:k-w', 3, 2), res

    @pytest.mark.skip('not implemented yet')
    def test_here_string(self):
        code = '''#<<abcdefg
yay some text!
abcdefg
'''
        racket.exp(code)
        ok, v, rest = res = racket.exp(code)
        assert ok and not rest, res


class TestPyrRacket(unittest.TestCase):

    def test_quote_sexp(self):
        hrm = pyru.RacketParser("'(i am a teapot short and stout)")
        ap = hrm.asPython()
        assert ap == ('quote', ('i', 'am', 'a', 'teapot', 'short', 'and', 'stout'))

    def test_quote_str(self):
        hrm = pyru.RacketParser(""" '"i am redundant" """)
        ap = hrm.asPython()
        assert ap == "i am redundant"

    def test_add(self):
        hrm = pyru.RacketParser('(+ 1 2)')
        ap = hrm.asPython()
        assert ap == 3

    def test_complex(self):
        test = """
(protc:executor-verb "diluted" (hyp: 'ycN7aDCqEemsREea4BwMxg)  ; https://hyp.is/ycN7aDCqEemsREea4BwMxg
  (protc:input "blocking buffer" (hyp: 'L9TvjkRTEem4q5dY1oVi-w))  ; https://hyp.is/L9TvjkRTEem4q5dY1oVi-w
  (protc:input "goat anti-VR1" (hyp: 'D91m9ippEemlRi84HUXyKw)  ; https://hyp.is/D91m9ippEemlRi84HUXyKw
    (protc:implied-aspect "dilution" (hyp: 'uN3DpiP6Eemt8lPtBr6wUg)  ; https://hyp.is/7nDe9iDOEem6X6NI4c_F3A
      (protc:invariant (param:ratio 1 150) (hyp: '7nDe9iDOEem6X6NI4c_F3A))))  ; https://hyp.is/7nDe9iDOEem6X6NI4c_F3A
  (protc:invariant (protc:fuzzy-quantity "overnight" "duration")
                    (hyp: 'B9_WniDPEemNW__FACccsA))  ; https://hyp.is/B9_WniDPEemNW__FACccsA
  (protc:parameter* (param:quantity 4 (param:unit 'degrees-celsius)) (hyp: 'DOeIYiDPEem7zPuTeuEhyA)))  ; https://hyp.is/DOeIYiDPEem7zPuTeuEhyA"""
        hrm = pyru.RacketParser(test)
        ap = hrm.asPython()
