Position = tuple[int,int]

board = [
    [None, None, '1', None, None],
    [None, '2' , '3', '4' , None],
    ['5' , '6' , '7', '8' , '9' ],
    [None, 'A' , 'B', 'C' , None],
    [None, None, 'D', None, None]
]

validPositions = [
                  (2,0),
           (1,1), (2,1), (3,1),
    (0,2), (1,2), (2,2), (3,2), (4,2),
           (1,3), (2,3), (3,3),
                  (2,4)
]

def check_position(position: Position) -> bool:
    return position in validPositions

def move_only(position: Position, direction: str) -> Position:
    match direction:
        case 'U':
            return (position[0], position[1] - 1)
        case 'D':
            return (position[0], position[1] + 1)
        case 'L':
            return (position[0] - 1, position[1])
        case 'R':
            return (position[0] + 1, position[1])
        case _:
            raise ValueError('You made a bad assumption somewhere...')

def move(position: Position, direction: str) -> Position:
    testMove = move_only(position, direction)
    if check_position(testMove):
        return testMove
    else:
        return position

def calc_code(position: Position) -> str:
    result = board[position[0]][position[1]]

    if result is None:
        raise Exception("This shouldn't be possible, check your logic!")
    else:
        return result

def run_line(position: Position, line: str) -> tuple[Position, str]:
    for direction in line:
        position = move(position, direction)

    return position, calc_code(position)

def run_lines(position: Position, lines: str) -> str:
    result = ''

    for line in lines.split('\n'):
        (position, code) = run_line(position, line)
        result += code

    return result

testInput = '''ULL
RRDDD
LURDL
UUUUD'''
testResult = run_lines((0,2), testInput)
print(f'test data result: {testResult}')

f = open('2016\\Day02\\input.txt', 'r')
lines = f.read()
result = run_lines((0,2), lines)

print(f'actual result: {result}')