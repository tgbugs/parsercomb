

def coln(n, iterable):
    """ Return an iterator on the nth column. """
    for rec in iterable:
        yield rec[n]
