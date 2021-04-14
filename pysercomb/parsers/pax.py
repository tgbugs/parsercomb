#!/usr/bin/env python3
""" parse Paxinos atlas index formats """
from pysercomb.parsing import *

LQUOTE = '‘'
RQUOTE = '’'
COMMA = ','

lquote = COMP(LQUOTE)
rquote = COMP(RQUOTE)
comma = COMP(COMMA)

cs = JOINT(comma, space)

wsoc = OR(comment, whitespace)
wsoc1 = OR(comment, whitespace1)
word = joinstr(MANY1(NOT(whitespace1)))

def make_figalph(v):
    return RETURN(tuple(f'{v[0]}{_}' for _ in v[1]))

def flatten_alph(v):
    out = tuple(i for e in v
                for i in (e
                          if (type(e) == tuple and
                              (any(type(_) == str or type(_) == tuple for _ in e) or
                               len(e) <= 1))
                          else ((e,) if e else e)))
    return RETURN(out)

figure_range = JOINT(int_, COMPOSE(hyphen_minus, int_), join=False)
figure_alpha_list = BIND(JOINT(int_, BIND(JOINT(char,
                                                MANY(COMPOSE(comma, char))),
                                          flatten)),
                         make_figalph
                        )

figure = END(int_, OR(cs, newline, comment))

flist_atom = OR(figure_range,  # FIXME bad implementation use lookhead if possible
                figure_alpha_list,
                figure)

figures = BIND(JOINT(flist_atom,
                     MANY(COMPOSE(cs, flist_atom))),
               flatten_alph)

abrev = END(word, space)
sbrev = END(word, OR(newline, comment))
alabel = joinstr(BIND(JOINT(word,
                            BIND(MANY(JOINT(END(space,
                                                NOT(flist_atom)),
                                            word)),
                                 flatten1)),
                      flatten))
slabel = joinstr(BIND(JOINT(word,
                            BIND(MANY(JOINT(END(space,
                                                NOT(sbrev)),
                                            word)),
                                 flatten1)),
                      flatten))

los = RETVAL(LEXEME(COMP('List of Structures')), 's')
loa = RETVAL(LEXEME(COMP('List of Abbreviations')), 'a')

arec = LEXEME(JOINT(abrev, COMPOSE(space, alabel), COMPOSE(space, figures)))  # FIXME Ce has no structure name
srec = LEXEME(JOINT(slabel, COMPOSE(space, sbrev)))

def section_out(v):
    label, many, last = v
    out = label, many + (last,)
    return RETURN(out)

section = BIND(OR(JOINT(los,
                        MANY1(END(srec,
                                  NOT(OR(loa, EOF)))),
                        srec),
                  JOINT(loa,
                        MANY1(END(arec,
                                  NOT(OR(los, EOF)))),
                        arec)),
               section_out)
sections = JOINT(section, section)

def main():
    tests = dict(
    atest4_0 = 'FAKE fake fake fake 117a    ; testing',
    atest4_1 = 'CeCv central cervical nucleus 117a,b',
    atest4_2 = 'Cg1 cingulate cortex, area 1 7-24, 79-82',
    atest4_3 = 'AA anterior amygdaloid area 25, 84-87, 93-95',
    atest4_4 = 'mfba medial forebrain bundle, ‘a’ component 9-21, 81-85',


    stest4_0 = 'ventromedial hypothalamic nucleus, central part VMHC\nnext a  ; testing\n  ',
    stest4_1 = 'ventromedial hypothalamic nucleus, central part VMHC  ; testing\n  ',
    stest4_2 = 'ventromedial hypothalamic nucleus, central part VMHC',
    stest4_3 = 'olfactory ventricle (olfactory part of lateral ventricle) OV'    ,

    stest6_1 = '4th and 5th cerebellar lobules 4/5Cb',
    atest6_1 = '9a,bCb 9th cerebellar lobule, a and b 140-159, 165',
    )

    ats = [(v, arec(v)) for k, v in tests.items() if k.startswith('a')]
    sts = [(v, srec(v)) for k, v in tests.items() if k.startswith('s')]

if __name__ == '__main__':
    main()
