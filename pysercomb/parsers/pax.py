#!/usr/bin/env python3.6
from pysercomb.parsing import *

LQUOTE = '‘'
RQUOTE = '’'
COMMA = ','

lquote = COMP(LQUOTE)
rquote = COMP(RQUOTE)
comma = COMP(COMMA)

cs = JOINT(comma, space)

figure_range = JOINT(int_, COMPOSE(hyphen_minus, int_), join=False)
figure = int_
flist_atom = OR(figure_range, figure)  # FIXME bad implementation use lookhead if possible
figures = JOINT(flist_atom,
                OR(MANY1(COMPOSE(cs,
                                 flist_atom)),
                   EOF))
abrev = END(MANY1(NOT(space)), space)
sbrev = END(MANY1(NOT(space)), EOF)
alabel = MANY1(END(MANY1(NOT(space)), figures))
slabel = END(MANY1(JOINT(MANY1(NOT(space)), space)), sbrev)  # rerererecursion!

sec_abrevs = None
sec_structs = None

arec = JOINT(abrev, alabel, figures)
srec = JOINT(slabel, abrev)

def main():
    tests = dict(
    atest4_1 = 'CeCv central cervical nucleus 117a,b',
    atest4_2 = 'Cg1 cingulate cortex, area 1 7-24, 79-82',
    atest4_3 = 'AA anterior amygdaloid area 25, 84-87, 93-95',
    atest4_4 = 'mfba medial forebrain bundle, ‘a’ component 9-21, 81-85',

    stest4_1 = 'ventromedial hypothalamic nucleus, central part VMHC',
    stest4_2 = 'olfactory ventricle (olfactory part of lateral ventricle) OV'    ,

    stest6_1 = '4th and 5th cerebellar lobules 4/5Cb',
    atest6_1 = '9a,bCb 9th cerebellar lobule, a and b 140-159, 165',
    )

    ats = [v for k, v in tests.items() if k.startswith('a')]
    sts = [v for k, v in tests.items() if k.startswith('s')]
    embed()

if __name__ == '__main__':
    main()
