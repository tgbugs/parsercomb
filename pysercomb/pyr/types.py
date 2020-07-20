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

    def asJson(self):
        # TODO
        return {'node_type': self.__class__.__name__}
