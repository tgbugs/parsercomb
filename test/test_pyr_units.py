import unittest
from pysercomb.parsers.units import DEGREES_FEAR
from pysercomb.pyr.units import ParamParser, ProtcParameter
from .common import *

evil_white_dot = DEGREES_FEAR.decode()

class TestParam(unittest.TestCase):
    def test_all(self):
        bads = []
        roundtrip = []
        for ir in parsed:
            try:
                unit = ParamParser(ir)()
                text = str(unit)
                if text:
                    new_ir = parameter_expression(text)
                    roundtrip.append((ir, new_ir))
                    if evil_white_dot in text:
                        bads.append(('', ProtcParameter(ir), ir, text))
                    else:
                        pass
            except BaseException as e:
                bads.append((repr(e), ProtcParameter(ir), ir, ''))

        assert not bads, '\n'.join([f'{e}\n{pp}\n{ir}\n{text}' for e, pp, ir, text in bads])

    def test_roundtrip(self):
        # mostly seeing order inversion issues and unit vs unit-expression
        roundtrip = []
        for ir in parsed:
            try:
                text = ParamParser(ir)()
                if text:
                    _, new_ir, _ = parameter_expression(text)
                    roundtrip.append((text, ir, new_ir))
            except BaseException as e:
                pass

        bads = [(text, ProtcParameter(a), ProtcParameter(b)) for text, a, b in roundtrip
                if a != b]

        join = '\n===============================================\n'
        assert not bads, '\n' + join.join([f'{text!r}\n{pa}\n{pb}' for text, pa, pb in bads])
