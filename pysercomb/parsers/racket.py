from pysercomb.parsing import *

# racket
def exp(p):
    return _exp(p)
newline = COMP('\n')
whitespace_atom = OR(COMP(' '), COMP('\t'), newline)
#whitespace_atom = OR(_whitespace_atom, END(_whitespace_atom, EOF))
whitespace = MANY(whitespace_atom)
whitespace1 = MANY1(whitespace_atom)  # FIXME this is broken to negation? (extremely slow)
comment = COMPOSE(whitespace,
                  COMPOSE(COMP(';'),
                          MANY(NOT(newline))))  # leave the newline intact
def LEXEME(func):
    return COMPOSE(OR(SKIP(comment, whitespace), whitespace), SKIP(func, OR(comment, whitespace)))
open_paren = LEXEME(COMP('('))
close_paren = LEXEME(COMP(')'))
quote_symbol = COMP("'")
double_quote_symbol = COMP('"')
_string = COMPOSE(double_quote_symbol,
                  SKIP(MANY(NOT(double_quote_symbol)),
                       double_quote_symbol))  # TODO escape
string = LEXEME(joinstr(_string))
symbol = OR(char, digit, COMP('-'), COMP('_'), colon, COMP('*'),
            NOT(OR(COMP('('),
                   COMP(')'),
                   quote_symbol,
                   double_quote_symbol,
                   COMP(';'),
                   whitespace1,
                   point,
                   EOF)))
#NIL = RETVAL(COMP("'()"), None)
num_literal = OR(scientific_notation, float_, int_)
cons_pair = COMPOSE(open_paren, JOINT(SKIP(exp, point), SKIP(exp, close_paren)))
literal = OR(num_literal, string)
atom = joinstr(MANY1(symbol))
identifier = LEXEME(atom)
def _quote(p):
    return OR(RETVAL(COMP('()'), None), cons_pair, COMPOSE(quote_symbol, _exp))(p)
quote = LEXEME(_quote)
def sexp(p):
    return sexp_inner(p)
_exp = LEXEME(OR(literal, identifier, quote, sexp))
sexp_inner = COMPOSE(open_paren, SKIP(MANY1(exp), close_paren))
lang_line = JOINT(COMP('#lang'), SKIP(MANY(NOT(COMP('\n'))), COMP('\n')))
racket_doc = COMPOSE(AT_MOST_ONE(lang_line), MANY(exp))
tag_doc = SKIP(JOINT(COMPOSE(open_paren,
                             COMP('tag-doc')),
                     quote,
                     quote,
                     string,),
               close_paren)
tag_docs = MANY1(tag_doc)


def main():
    import os
    print(identifier('hello world!'))
    print(quote('\'hello world!'))
    print(quote('\'"hello" world!'))
    print(string('"ARE YOU KIDDING ME \n NO???"'))
    print(tag_doc('(tag-doc \'tag "a b c")\n'))
    with open(os.path.expanduser('~/git/protc/protc-tags.rkt'), 'rt') as f:
        text = f.read()
    with open(os.path.expanduser('~/git/protc/protc-lib/protc/units/si-units-extras.rkt'), 'rt') as f:
        text2 = f.read()
    td = tag_docs(text)
    e = racket_doc(text2)


if __name__ == '__main__':
    main()
