import re
from setuptools import setup


def find_version(filename):
    _version_re = re.compile(r"__version__ = '(.*)'")
    for line in open(filename):
        version_match = _version_re.match(line)
        if version_match:
            return version_match.group(1)


__version__ = find_version('pysercomb/__init__.py')

with open('README.md', 'rt') as f:
    long_description = f.read()

rdf_require = ['neurdflib', 'pyontutils']
units_require = ['protcur']
tests_require = ['pytest', 'pytest-runner'] + rdf_require + units_require
setup(name='pysercomb',
      version=__version__,
      description='parser combinator library and assorted parsers',
      long_description=long_description,
      long_description_content_type='text/markdown',
      url='https://github.com/tgbugs/parsercomb',
      author='Tom Gillespie',
      author_email='tgbugs@gmail.com',
      license='MIT',
      classifiers=[
          'Development Status :: 4 - Beta',
          'License :: OSI Approved :: MIT License',
          'Programming Language :: Python :: 3.6',
          'Programming Language :: Python :: 3.7',
      ],
      keywords=('python parsing parser combinator scientific '
                'units parsec protc'),
      packages=[
          'pysercomb',
          'pysercomb.parsers',
          'pysercomb.pyr',
      ],
      package_data={'pysercomb.pyr': ['pyr_units.txt']},
      python_requires='>=3.6',
      tests_require=tests_require,
      install_requires=['pint', 'uncertainties', 'babel'],  # need to fix pint deps
      extras_require={'units': units_require,
                      'rdf': rdf_require,
                      'test': tests_require,
                     },
      scripts=[],
      entry_points={'console_scripts': [ ],},
      data_files=[]  # TODO
     )
