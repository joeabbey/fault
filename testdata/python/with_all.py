"""Module with explicit __all__ declaration."""

__all__ = ['public_func', 'PublicClass']


def public_func():
    """This is public via __all__."""
    pass


def another_func():
    """This is NOT in __all__, so not exported."""
    pass


class PublicClass:
    """In __all__."""
    pass


class AnotherClass:
    """Not in __all__."""
    pass
