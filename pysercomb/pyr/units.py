""" Python class representation for the output of the units parser. """
import pprint


class ProtcParameter:
    format_nl =  '*', '/', 'range', 'plus-or-minus', 'param:dimensions'
    format_nl_long =  '^'
    def __init__(self, tuple_repr):
        self._tuple = tuple_repr

    def isLongNL(self, tuple_):
        if tuple_[0] in self.format_nl_long:
            t1 = type(tuple_[1]) is tuple
            t2 = type(tuple_[2]) is tuple
            if t1 and t2:
                if len(tuple_[1]) > 2 or len(tuple_[2]) > 2:
                    return True
            elif t1 and len(tuple_[1]) > 3:
                return true
            elif t2 and len(tuple_[2]) > 3:
                return true
        return False

    def format_value(self, localIndent=0, depth=0, tuple_=None):#, LID=''):
        if tuple_ is None:
            tuple_ = self._tuple
            #from IPython import embed
            #import sys
            #embed()
            #sys.exit(0)

        out = ''
        if tuple_:
            newline = tuple_[0] in self.format_nl or self.isLongNL(tuple_)
            indent_for_this_loop = localIndent + len('(') + len(tuple_[0]) + len(' ')  # vim fail )
            indent_for_next_level = indent_for_this_loop
            #indent_this = LID + '(' + tuple_[0] + ' '  # vim fail )
            #indent_next = indent_this
            for i, v in enumerate(tuple_):
                if newline and i > 1:
                    out += '\n' + ' ' * indent_for_this_loop
                    #out += '\n' + indent_this
                if type(v) is tuple:
                    v = self.format_value(indent_for_next_level, depth + 1, v)#, LID=indent_next)
                if v is not None:
                    v = str(v)
                    if out and out[-1] != ' ':
                        out += ' ' + v
                        if i > 1 or not newline:
                            indent_for_next_level += len(' ') + len(v) # unlike at the top v already has ( prepended if it exists
                            #indent_next += ' ' + v
                    else:  # we are adding indents
                        out += v
        if out:
            return '(' + out + ')'

    def _repr_pretty_(self, p, cycle):
        if cycle:
            p.text(self.__class__.__name__ + '(WAT...)')
        else:
            indent = p.output_width
            p.text(self.__repr__(indent))

    def __repr__(self, indent=0):
        cname = self.__class__.__name__
        _tuple = self.format_value(len(cname) + 1 + indent) 
        return cname + f'({_tuple})'
        

def _pprint_operation(self, object, stream, indent, allowance, context, level):
    #value = object.format_value(indent)  # how the heck does this work?
    value = object.__repr__(indent)  # how the heck does this work?
    stream.write(value)


pprint.PrettyPrinter._dispatch[ProtcParameter.__repr__] = _pprint_operation
