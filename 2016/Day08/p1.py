from dataclasses import dataclass
from typing import TypeVar


@dataclass
class Screen:
    width: int
    height: int
    pixels: list[list[bool]]

def newScreen(width: int, height: int) -> Screen:
    pixels = [[False for _ in range(width)] for _ in range(height)]
    return Screen(width, height, pixels)

A = TypeVar('A')

def transpose(toTranspose: list[list[A]]) -> list[list[A]]:
    """Transpose a list of lists."""

    result: map[list[A]] = map(list, zip(*toTranspose))
    return list(result)

def concat(list1: list[A], list2: list[A]) -> list[A]:
    """Alternative to `list.extend` for merging/flattening that returns the resulting list."""

    list1.extend(list2)
    return list1

def rotateList(lst: list[A], distance: int, reverse: bool = False) -> list[A]:
    """Rotate a list to the right by `index` values. Rotates left if `reverse` is `True`."""

    if not reverse:
        distance = len(lst) - distance

    return concat(lst[distance:], lst[:distance])

def rect(screen: Screen, width: int, height: int) -> Screen:
    """Set pixels to ON in a grid from the top-left of the screen."""

    for y in range(height):
        for x in range(width):
            screen.pixels[y][x] = True

    return screen

def rotateRow(screen: Screen, row: int, distance: int) -> Screen:
    """Rotate pixels in `row` by `distance`."""

    screen.pixels[row] = rotateList(screen.pixels[row], distance)
    return screen

def rotateColumn(screen: Screen, column: int, distance: int) -> Screen:
    """Rotate pixels in `column` by `distance`."""

    transposed = transpose(screen.pixels)
    transposed[column] = rotateList(transposed[column], distance)
    screen.pixels = transpose(transposed)

    return screen

def printScreen(screen: Screen) -> None:
    def pixelToString(pixel: bool) -> str:
        if pixel:
            return '#'
        return '.'

    for row in screen.pixels:
        print(''.join(map(pixelToString, row)))

    print()

"""
screen = newScreen(7, 3)
printScreen(screen)

screen = rect(screen, 3, 2)
printScreen(screen)

screen = rotateColumn(screen, 1, 1)
printScreen(screen)

screen = rotateRow(screen, 0, 4)
printScreen(screen)

screen = rotateColumn(screen, 1, 1)
printScreen(screen)

.......
.......
.......

###....
###....
.......

#.#....
###....
.#.....

....#.#
###....
.#.....

.#..#.#
#.#....
.#.....
"""

# print(rotateList(list(range(10)), 2))