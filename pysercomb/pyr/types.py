# types that can be imported quickly


class Unit:
    """ sigh python """


class Quantity:
    """ sigh python """
    tag = 'quantity'


class Range:
    """ sigh python """
    tag = 'range'


class ProtcurExpression:
    """ sigh python """


class AJ:
    """ As json helper """

    @property
    def rchildren(self):  # FIXME this does not go here
        if hasattr(self, 'body'):
            for child in self.body:
                yield child
                yield from child.rchildren

    def asJson(self):
        # TODO
        if self.prov is None:
            breakpoint()
        else:
            if hasattr(self.prov, 'id'):
                # hypothesis helper case
                id = self.prov.id
            elif isinstance(self.prov, str):
                # string identifier case
                id = self.prov
            #elif hasattr(self, '_id'):
                #id = self._id
            else:
                # FIXME
                # is ast node ...
                # which is kind of AJ?
                # also consider using _id?
                id = self._value
                breakpoint()

        out = {
            '@id': id,
            '@type': self._type,
            '@value': self._value,
            'node_type': self.__class__.__name__,  # TODO @type is what?
        }

        if hasattr(self, 'body'):
            out['children'] = [node.asJson() for node in self.body]

        # TODO nest children in the jsonld and then hope it flattens?
        return out
