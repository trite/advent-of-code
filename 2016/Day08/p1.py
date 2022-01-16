from typing import TypeVar

A = TypeVar('A')

def concat(list1: list[A], list2: list[A]) -> list[A]:
    """Alternative to `list.extend` for merging/flattening that returns the resulting list."""

    list1.extend(list2)
    return list1

def rotateList(lst: list[A], index: int, reverse: bool = False) -> list[A]:
    """Rotate a list to the right by `index` values. Rotates left if `reverse` is `True`."""

    if not reverse:
        index = len(lst) - index

    return concat(lst[index:], lst[:index])

print(rotateList(list(range(10)), 2))