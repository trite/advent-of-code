
from typing import TypeVar

# def TODO(info: str | None = None) -> None:
#     raise Exception(f'This is not yet implemented. {info}')

# TODO = lambda: raise Exception('')

A = TypeVar('A')

# `Optional generic input -> generic output` to keep typing simple for now
def TODO(retVal: A | None = None) -> A:
    raise NotImplementedError

def windowed(lst: list[A], size: int) -> list[list[A]]:
    return [lst[i:i+size] for i in range(len(lst) - size + 1)]