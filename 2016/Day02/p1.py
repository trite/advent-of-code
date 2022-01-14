from enum import IntEnum

class Horizontal(IntEnum):
    Left = 0
    Middle = 1
    Right = 2

class Vertical(IntEnum):
    Top = 1
    Middle = 4
    Bottom = 7

class State:
    horizontal: Horizontal = Horizontal.Middle
    vertical: Vertical = Vertical.Middle

def move(state: State, direction: str) -> State:
    match direction:
        case 'U' if state.vertical > 1:
            state.vertical = Vertical(state.vertical - 3)
        case 'D' if state.vertical < 7:
            state.vertical = Vertical(state.vertical + 3)
        case 'L' if state.horizontal > 0:
            state.horizontal = Horizontal(state.horizontal - 1)
        case 'R' if state.horizontal < 2:
            state.horizontal = Horizontal(state.horizontal + 1)
        case _:
            pass

    return state

def calc_number(state: State) -> int:
    return state.horizontal + state.vertical

def run_line(state: State, line: str) -> tuple[State,int]:
    for direction in line:
        state = move(state, direction)

    return state, calc_number(state)

def run_lines(state: State, lines: str) -> int:
    result = ''

    for line in lines.split('\n'):
        (state, x) = run_line(state, line)
        result += str(x)

    return result

testInput = '''ULL
RRDDD
LURDL
UUUUD'''
testResult = run_lines(State(), testInput)
print(f'test data result: {testResult}')

f = open('2016\\Day02\\input.txt', 'r')
lines = f.read()
result = run_lines(State(), lines)

print(f'actual result: {result}')
# test data result: 1985
# actual result: 95549