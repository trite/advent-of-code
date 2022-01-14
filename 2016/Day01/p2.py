from enum import IntEnum
from dataclasses import dataclass

Coords = tuple[int,int]

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
    coords: Coords = (0,0)
    prevPlaces: set[Coords] = set()

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
        case "R":
            return Instruction(Relative.Right, rest)
        case "L":
            return Instruction(Relative.Left, rest)
        case _:
            raise ValueError("You made a bad assumption somewhere...")

def calcDistance(state: State) -> int:
    return abs(state.coords[0]) + abs(state.coords[1])

def transit(state: State, translation: Coords, distance: int) -> State:
    for i in range(distance):
        state.coords = (state.coords[0] + translation[0],
            state.coords[1] + translation[1])

        if state.prevPlaces.__contains__(state.coords):
            print(f'Revisiting {state.coords}, for a distance of {calcDistance(state)}')
            quit()

        state.prevPlaces.add(state.coords)
        
    return state

def applyInstruction(state: State, instruction: Instruction) -> State:
    state.direction = changeDirection(state.direction, instruction.turn)

    match state.direction:
        case Cardinal.North:
            state = transit(state, (0,1), instruction.distance)
        case Cardinal.East:
            state = transit(state, (1,0), instruction.distance)
        case Cardinal.South:
            state = transit(state, (0,-1), instruction.distance)
        case Cardinal.West:
            state = transit(state, (-1,0), instruction.distance)

    return state


    
f = open("2016\\Day01\\input.txt", "r")
content = f.read()
f.close()

commands = content.split(", ")
instructions = [parse(command) for command in commands]

state = State()
for instruction in instructions:
    state = applyInstruction(state, instruction)

print(calcDistance(state))
# Revisiting (-176, -5), for a distance of 181
# answer: 181