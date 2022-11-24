# types that can be imported quickly


class Unit:
    """ sigh python """


class Quantity:
    """ sigh python """
    tag = 'quantity'


class Measurement:
    """ sigh python """
    tag = 'measurement'


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
                # reasonable compromise with using curie
                # FIXME evil because it conflates the
                # hypothesis view with the protcur view
                # could use them as blanknodes but tricky then
                # FIXME this should not be on the generic AJ class ...
                id = 'hyp-protcur:' + self.prov.id
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

        #context = {  # doesn't go here
            #'children': {'@id': 'ilxtr:protcurBody' , '@type': '@id'},
        #}

        out = {
            #'@context': context,
            '@id': id,
            '@type': self._type,
            'node_type': self.__class__.__name__,  # TODO @type is what?
        }

        value = self._value
        if isinstance(value, str) and value.startswith('aspect-raw:'):  # FIXME hardcoding
            out['value'] = value
        elif (isinstance(value, str) or
              isinstance(value, int) or
              isinstance(value, float)):
        #elif not (isinstance(value, dict) or
                  #isinstance(value, list) or
                  #value.__class__.__name__ == 'Term'  # FIXME DUMB
                  #):
            out['raw_value'] = {'@value': value}
        else:
            out['ast_value'] = value

        if 'raw_value' not in out and hasattr(value, 'original'):
            # lift original values back to their original position in the ast
            out['raw_value'] = {'@value': value.original}

        if self.prov and hasattr(self.prov, 'id'):  # FIXME sigh assumptions about prov
            uris = {
                'uri_human_context': self.prov.shareLink,
                'uri_human_static': self.prov.htmlLink,
                'uri_api': 'https://hypothes.is/api/annotations/' + self.prov.id,
            }
            out.update(uris)
            if self.__class__.__name__ in ('Parameter', 'Invariant'):  # FIXME sigh
                out['raw_value'] = {'@value': self.prov._value()}
                if (self.rest and
                    (isinstance(self.rest[0], list) or isinstance(self.rest[0], tuple)) and
                    self.rest[0] and
                    self.rest[0][0] == "rest"):
                    # TODO mark out as a whole as an error probably
                    out['rest'] = self.rest[0][1]

        if hasattr(self, 'body'):
            if self.body:
                try:
                    body = [node.asJson() for node in self.body]  # FIXME order semantics
                except AttributeError as e:
                    breakpoint()
                    'asdf'
                out['children'] = [b['@id'] for b in body if '@id' in b]

        # TODO nest children in the jsonld and then hope it flattens?
        return out

    def __str__(self):
        """ you proably shouldn't use this ... """
        return str(self._value)
