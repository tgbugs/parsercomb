import unittest
from pysercomb.parsers import racket
from pysercomb.parsing import float_, int_, scientific_notation


class TestForms(unittest.TestCase):
    def test_int(self):
        res = int_('1')
        _, v, _  = res
        assert v == 1, res

    def test_scinot(self):
        _, v, _ = scientific_notation('1E10')
        assert v == '1E10', v

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
