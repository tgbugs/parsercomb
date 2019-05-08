class PyrError(Exception):
    """ base error class """


class BadNotationError(PyrError):
    """ the parser detected a misuse of a notation """


class ParseFailure(PyrError):
    """ we failed to parse an expression """
