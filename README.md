# pysercomb
[![PyPI version](https://badge.fury.io/py/pysercomb.svg)](https://pypi.org/project/pysercomb/)
[![Build Status](https://travis-ci.org/tgbugs/parsercomb.svg?branch=master)](https://travis-ci.org/tgbugs/parsercomb)
[![Coverage Status](https://coveralls.io/repos/github/tgbugs/parsercomb/badge.svg?branch=master)](https://coveralls.io/github/tgbugs/parsercomb?branch=master)

python parser combinator and parsers

## Units
The most useful thing in this repo is probably the units parser which
has been tuned to extract scientific units from the published literature.

### protc dependency
If you want to use units parser you will need to have a copy of the [protc repo](https://github.com/tgbugs/protc)
and have the repo version protcur installed. See [.travis.yml](./.travis.yml) for details.
The units parser and protc are still tightly coupled for the time being.
