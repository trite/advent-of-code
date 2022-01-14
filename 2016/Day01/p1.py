from enum import IntEnum
from dataclasses import dataclass

class Cardinal(IntEnum):
    North = 0
    East = 1
    South = 2
    West = 3

class Relative(IntEnum):
    Left = -1
    Right = 1

class State:
    direction: Cardinal = Cardinal.North
    coords: tuple[int,int] = (0,0)

@dataclass
class Instruction:
    turn: Relative
    distance: int

def changeDirection(direction: Cardinal, turn: Relative) -> Cardinal:
    updated = direction + turn

    # wrapping around:
    if updated < 0:
        updated = 3
    elif updated > 3:
        updated = 0

    return updated

def parse(instruction: str) -> Instruction:
    rest = int(instruction[1:])
    match instruction[0]:
        case 'R':
            return Instruction(Relative.Right, rest)
        case 'L':
            return Instruction(Relative.Left, rest)
        case _:
            raise ValueError('You made a bad assumption somewhere...')

def applyInstruction(state: State, instruction: Instruction) -> State:
    state.direction = changeDirection(state.direction, instruction.turn)

    match state.direction:
        case Cardinal.North:
            state.coords = (state.coords[0], state.coords[1] + instruction.distance)
        case Cardinal.East:
            state.coords = (state.coords[0] + instruction.distance, state.coords[1])
        case Cardinal.South:
            state.coords = (state.coords[0], state.coords[1] - instruction.distance)
        case Cardinal.West:
            state.coords = (state.coords[0] - instruction.distance, state.coords[1])

    return state

def calcDistance(state: State) -> int:
    return abs(state.coords[0]) + abs(state.coords[1])



f = open('2016\\Day01\\input.txt', 'r')
content = f.read()
f.close()

commands = content.split(', ')
instructions = [parse(command) for command in commands]

testing = parse('R2')

state = State()
for instruction in instructions:
    state = applyInstruction(state, instruction)

print(calcDistance(state))
# answer: 299