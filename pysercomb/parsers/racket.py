from pysercomb.parsing import *

# racket
def exp(p):
    return _exp(p)

comment = COMPOSE(whitespace,
                  COMPOSE(COMP(';'),
                          MANY(NOT(newline))))  # leave the newline intact
def LEXEME(func):
    return COMPOSE(OR(SKIP(MANY1(comment), whitespace), whitespace),
                   SKIP(func, OR(MANY1(comment), whitespace)))
open_paren = LEXEME(COMP('('))
close_paren = LEXEME(COMP(')'))
quote_symbol = COMP("'")
double_quote_symbol = COMP('"')
_string_escape = RETVAL(COMP('\\"'), '"')
_string = COMPOSE(double_quote_symbol,
                  SKIP(MANY(OR(_string_escape, NOT(double_quote_symbol))),
                       # string escape has to go first so the \ arent eaten
                       double_quote_symbol))  # TODO escape
string = LEXEME(joinstr(_string))
#here_string = COMPOSE(OR(newline, BOF), COMP("#<<"), pattern .+ COMPOSE(pattern, newline))
# XXX I don't know if we have a stack in here to store the pattern ...
# actually it is probably easier in that we can just create a new combinator
# for use inside the function itself ... need to review
symbol = OR(char, digit, COMP('-'), COMP('_'), colon, COMP('*'),
            NOT(OR(COMP('('),
                   COMP(')'),
                   quote_symbol,
                   double_quote_symbol,
                   COMP(';'),
                   whitespace1,
                   point,
                   EOF)))
empty_list = RETVAL(COMP("'()"), None)
true = RETVAL(COMP("#t"), True)
false = RETVAL(COMP("#f"), False)
num_literal = END(OR(scientific_notation, float_, int_), NOT(symbol))  # FIXME HACK
cons_pair = COMPOSE(open_paren, JOINT(SKIP(exp, point), SKIP(exp, close_paren)))
literal = OR(num_literal, string, true, false, empty_list)
atom = joinstr(MANY1(symbol))  # not quite right due to numbers
identifier = LEXEME(atom)

# Include the explicit quote since we transform everything into strings
# since python has no symbols. Eval rules should not be applied here.
# They should be applied later in a second step to obtain the python ir.
quote = LEXEME(BIND(COMPOSE(quote_symbol, exp), lambda v: RETURN(('quote', v))))
def sexp(p):
    return sexp_inner(p)
# It is consisten with racket behavior for unquoted cons pairs e.g. (1 . 2)
# to read. They error during #%app since they are not quoted
_exp = LEXEME(OR(literal, identifier, quote, sexp, cons_pair))
sexp_inner = COMPOSE(open_paren, SKIP(MANY(exp), close_paren))  # not MANY1 because () is not a parse error
lang_line = COMPOSE(COMP('#lang'), SKIP(COMPOSE(whitespace1, MANY1(atom)), COMP('\n')))
module_from_lang = BIND(ANDTHEN(EXACTLY_ONE(lang_line),
                                BIND(MANY(exp), lambda v: RETURN(('module-begin', *v)))),
                        lambda v: RETURN(('module-unexp', None, *v)))
#racket_doc = OR(COMPOSE(EXACTLY_ONE(lang_line), MANY(exp)), MANY1(exp))
racket_doc = OR(module_from_lang, MANY1(exp))
tag_doc = SKIP(JOINT(COMPOSE(open_paren,
                             COMP('tag-doc')),
                     quote,
                     quote,
                     string,),
               close_paren)
tag_docs = MANY1(tag_doc)


def racket_module(value):
    """ a very minimal racket -> python interpreter """
    _awaiting = object()
    _provide_expect = object()
    def car(tup): return tup[0]
    def cdr(tup): return tup[1:]
    def eval(env, tup):
        last = None
        for value in tup:
            if isinstance(value, tuple):
                if car(value) == 'module-unexp':
                    _, file, lang, rest = value
                    return eval(None, rest)
                elif car(value) == 'module-begin':
                    env = {_provide_expect: set()}
                    eval(env, cdr(value))
                    provide_expect = env.pop(_provide_expect)
                    out = {name:value for name, value in env.items() if name in provide_expect}
                    missing = provide_expect - set(out)
                    if missing:
                        raise ValueError(f'not provided {missing}')

                    return out
                elif car(value) == 'provide':
                    for v in cdr(value):  # FIXME too simple
                        env[_provide_expect].add(v)
                    last = None
                elif car(value) == 'define':
                    if isinstance(value[2], tuple) and len(value[2:]) > 1:
                        raise NotImplementedError('havent implemented functions yet')

                    env[value[1]] = eval(env, value[2:])
                elif car(value) == 'quote':
                    rest = value[1]
                    return rest

                last = None
            else:
                last = value

        return last

    return eval(None, value)


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
