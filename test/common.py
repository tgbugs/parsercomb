from pathlib import Path
from protcur.config import __units_folder__ as units_folder
from pysercomb.parsers import units

(parameter_expression, quantity, unit, *_,
 debug_dict) = units.make_unit_parser(units_folder)

*_, test_all, parsed = units.parse_for_tests(parameter_expression)

# evil
_gs = globals()
_gs.update(debug_dict)

