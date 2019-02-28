import os
from pathlib import Path
from setuptools import setup

with open('README.md', 'rt') as f:
    long_description = f.read()

setup(name='pysercomb',
      version='0.0.1',
      description='assorted',
      long_description=long_description,
      long_description_content_type='text/markdown',
      url='https://github.com/tgbugs/parsercomb',
      author='Tom Gillespie',
      author_email='tgbugs@gmail.com',
      license='MIT',
      classifiers=[],
      keywords='python parsing parser combinator units parsec protc',
      packages=['pysercomb'],
      install_requires=[ ],
      extras_require={'dev':[]},
      scripts=[],
      entry_points={'console_scripts': [ ],},
      data_files=[]  # TODO
     )
