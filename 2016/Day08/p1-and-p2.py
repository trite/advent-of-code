from dataclasses import dataclass
from typing import TypeVar

@dataclass
class Screen:
    width: int
    height: int
    pixels: list[list[bool]]

    def __init__(self, width: int, height: int) -> None:
        self.width = width
        self.height = height
        self.pixels = [[False for _ in range(width)] for _ in range(height)]

    def count(self) -> int:
        return sum([sum(x) for x in self.pixels])
        # return sum([[if self.pixels[y][x] for x in range(self.width)] for y in range(self.height)])

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
        # l = len(lst)
        distance = len(lst) - distance
        # distance = l - (l % distance)

    result = concat(lst[distance:], lst[:distance])

    return result

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

@dataclass
class CmdRect:
    width: int
    height: int

    def run(self, screen: Screen):
        return rect(screen, self.width, self.height)

@dataclass
class CmdRotateRow:
    y: int
    amount: int

    def run(self, screen: Screen):
        return rotateRow(screen, self.y, self.amount)

@dataclass
class CmdRotateColumn:
    x: int
    amount: int

    def run(self, screen: Screen):
        return rotateColumn(screen, self.x, self.amount)

ScreenCommand = CmdRect | CmdRotateRow | CmdRotateColumn

def parse_line(line: str) -> ScreenCommand:
    parts = line.split(' ')
    match parts:
        case ['rect', size]:
            x,y = size.split('x')
            return CmdRect(int(x), int(y))
        
        case ['rotate', 'column', x, 'by', amount]:
            return CmdRotateColumn(int(x.split('=')[1]), int(amount))

        case ['rotate', 'row', y, 'by', amount]:
            return CmdRotateRow(int(y.split('=')[1]), int(amount))

        case other:
            raise Exception(f"Couldn't parse: {other}")

def run(rawLines: str) -> int:
    commands = list(map(parse_line, rawLines.split('\n')))
    # screen = Screen(7, 3)
    screen = Screen(50, 6)

    for command in commands:
        screen = command.run(screen)

    return screen.count()



# testStr = """rect 3x2
# rotate column x=1 by 1
# rotate row y=0 by 4
# rotate column x=1 by 1"""
# print(run(testStr))

f = open('2016\\Day08\\input.txt', 'r')
content = f.read()
print(run(content))
# part 1 result: 123



def run_p2(rawLines: str) -> None:
    commands = list(map(parse_line, rawLines.split('\n'))) 
    screen = Screen(50, 6)

    for command in commands:
        screen = command.run(screen)

    printScreen(screen)

run_p2(content)
""" Part 2 result: AFBUPZBJPS
.##..####.###..#..#.###..####.###....##.###...###.
#..#.#....#..#.#..#.#..#....#.#..#....#.#..#.#....
#..#.###..###..#..#.#..#...#..###.....#.#..#.#....
####.#....#..#.#..#.###...#...#..#....#.###...##..
#..#.#....#..#.#..#.#....#....#..#.#..#.#.......#.
#..#.#....###...##..#....####.###...##..#....###..
"""