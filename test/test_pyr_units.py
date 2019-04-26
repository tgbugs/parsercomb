import unittest
from pysercomb.parsers.units import DEGREES_FEAR
from pysercomb.pyr.units import TextPP, ProtcParameter
from .common import *

evil_white_dot = DEGREES_FEAR.decode()

class TestTextPP(unittest.TestCase):
    def test_all(self):
        bads = []
        roundtrip = []
        for ir in parsed:
            try:
                text = TextPP(ir)()
                if text:
                    new_ir = parameter_expression(text)
                    roundtrip.append((ir, new_ir))
                    if evil_white_dot in text:
                        bads.append(('', ProtcParameter(ir), ir, text))
                    else:
                        #print(text)
                        pass
            except BaseException as e:
                bads.append((e, ProtcParameter(ir), ir, ''))

        assert not bads, '\n'.join([f'{e}\n{pp}\n{ir}\n{text}' for e, pp, ir, text in bads])

    def test_roundtrip(self):
        # mostly seeing order inversion issues and unit vs unit-expression
        roundtrip = []
        for ir in parsed:
            try:
                text = TextPP(ir)()
                if text:
                    _, new_ir, _ = parameter_expression(text)
                    roundtrip.append((ir, new_ir))
            except BaseException as e:
                pass

        bads = [(ProtcParameter(a), ProtcParameter(b)) for a, b in roundtrip
                if a != b]

        join = '\n===============================================\n'
        assert not bads, join.join([f'{a}\n{b}' for a, b in bads])

    def test_prefix_unit(self):
        pass
        
