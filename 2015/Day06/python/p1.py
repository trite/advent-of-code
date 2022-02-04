from dataclasses import dataclass
from typing import Literal, Tuple
from PIL import Image, ImageDraw

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

imgFolder = '2015/Day06/python/images'

im = Image.new('RGB', (1000, 1000), (0, 0, 0))
draw = ImageDraw.Draw(im)

cmds = list(map(parse_line, testCmds))

for cmd in cmds:
    match cmd:
        case Command("On", start, end):
            fill = (255, 255, 255)
        case Command("Off", start, end):
            fill = (255, 255, 255)
        case Command("Toggle", start, end):
            fill = (255, 255, 255)
        case other:
            raise Exception(f"Unable to handle command! {other}")

# draw.rectangle((0, 0, 999, 999), fill=(255, 255, 255))

im.save(f'{imgFolder}/p1_test_01.jpg', quality=95)