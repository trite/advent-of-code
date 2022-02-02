from dataclasses import dataclass
from typing import Literal, Tuple
from PIL import Image
import numpy

test = Image.new(mode="L", size=(100,100))
# test.show()
# test2 = Image.fromarray(numpy.array("i", [x for x in range(10000)]), "L") # type: ignore

# size = 1000
# numpy.random.seed(1)
# arr = numpy.random.rand(size, size)
arr = numpy.fromfunction(lambda x, y: y / 1000, (1000, 1000), dtype = float) # type: ignore
Image.fromarray(arr, mode='L').show() # type: ignore

# test2.show()

Position = Tuple[int,int]

def parse_position(rawPair: str) -> Position:
    match rawPair.split(','):
        case [x,y]:
            return int(x),int(y)
        case other:
            raise Exception(f'Check your assumptions, this should always be a Tuple! {other}')

CommandType = Literal["On", "Off", "Toggle"]

@dataclass
class Command:
    cmdType: CommandType
    start: Position
    end: Position

    def __init__(self, cmd: CommandType, start: str, end: str):
        self.cmdType = cmd
        self.start = parse_position(start)
        self.end = parse_position(end)

testCmds = [
    'turn on 0,0 through 999,999',
    'toggle 0,0 through 999,0',
    'turn off 499,499 through 500,500',
]

def parse_line(line: str) -> Command:
    match line.split(' '):
        case ['turn', 'on', start, 'through', end]:
            return Command("On", start, end)

        case ['turn', 'off', start, 'through', end]:
            return Command("Off", start, end)

        case ['toggle', start, 'through', end]:
            return Command("Toggle", start, end)

        case other:
            raise Exception(f'Unable to parse: {other}')

