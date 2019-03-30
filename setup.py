import os
from pathlib import Path
from setuptools import setup

with open('README.md', 'rt') as f:
    long_description = f.read()

tests_require = ['pytest', 'pytest-cov']
setup(name='pysercomb',
      version='0.0.1',
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
                'units parsec protc',
      packages=[
          'pysercomb',
          'pysercomb.parsers',
          'pysercomb.pyr',
      ],
      python_requires='>=3.6',
      tests_require=tests_require,
      install_requires=[],
      extras_require={'dev': [],
                      'test': tests_require,
                     },
      scripts=[],
      entry_points={'console_scripts': [ ],},
      data_files=[]  # TODO
     )
