from typing import Any, Dict, Tuple, TypeVar

A = TypeVar('A')

# `Optional generic input -> generic output` to keep typing simple for now
def TODO(info: str | None = None, _: A | None = None) -> A:
    """
        Raises a NotImplementedError with optional message.
        Should match function signatures for easy return values too.

        Usage:
    """
    if info is not None:
        raise NotImplementedError(info)
    raise NotImplementedError

# TODO: Does 
def go_interactive(locs: Dict[str, Any]):
    """
        Enter interactive mode. Must call `locals()` from scope you want to use when entering interactive mode.

        Usage:
            go_interactive(locals())
    """
    # Cool interaction - entering interactive mode this way leaves debugging running.
    # Can then execute code interactively and breakpoints will be handled appropriately!
    import code
    code.interact(local=locs)

def windowed(lst: list[A], size: int) -> list[list[A]]:
    """
        >>> windowed(list(range(10)), 2)
        [[0, 1], [1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 7], [7, 8], [8, 9]]
        >>> windowed(list(range(10)), 4) 
        [[0, 1, 2, 3], [1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6], [4, 5, 6, 7], [5, 6, 7, 8], [6, 7, 8, 9]]
    """
    return [lst[i:i+size] for i in range(len(lst) - size + 1)]

def split_list_chunks(toSplit: list[A], count: int) -> list[list[A]]:
    """
        >>> split_list(list(range(10)), 5)
        [[0, 1, 2, 3, 4], [5, 6, 7, 8, 9]]
        >>> split_list(list(range(10)), 2) 
        [[0, 1], [2, 3], [4, 5], [6, 7], [8, 9]]
    """
    blah = [toSplit[i:i+count] for i in range(0, len(toSplit), count)]
    return blah

def split_list_bisect(toSplit: list[A], splitAt: int) -> Tuple[list[A], list[A]]:
    """
        >>> split_list_bisect(list(range(10)), 2)
        ([0, 1], [2, 3, 4, 5, 6, 7, 8, 9])
    """
    return toSplit[:splitAt], toSplit[splitAt:]

if __name__ == '__main__':
    go_interactive(locals())