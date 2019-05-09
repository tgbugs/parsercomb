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

    def test_nil(self):
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
        _, v, _ = res
        print(res)
        assert v == (('provide', 'something'),
                     ('define', 'something',
                      (('a', 'b'),
                       (None, 0),
                       ('c', 1)))), v
