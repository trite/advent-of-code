from typing import Set, Tuple

Position = Tuple[int,int]

def apply_dir(dir: str, pos: Position) -> Position:
    x,y = pos
    match dir:
        case '^':
            return (x, y+1)
        case '>':
            return (x+1, y)
        case 'v':
            return (x, y-1)
        case '<':
            return (x-1, y)
        case other:
            raise Exception(f"Unable to parse the provided direction. {other}")

def run_p1(dirs: str) -> int:
    position = (0,0)
    result: Set[Position] = set([position])

    for dir in dirs:
        position = apply_dir(dir, position)
        result.add(position)

    return len(result)

# print(run_p1('>'))
# print(run_p1('^>v<'))
# print(run_p1('^v^v^v^v^v'))

with open('2015\\Day03\\input.txt', 'r') as f:
    content = f.read()

print(f'P1 answer: {run_p1(content)}')
# P1 answer: 2081

def run_p2(dirs: str) -> int:
    santa = (0,0)
    robot = (0,0)
    result: Set[Position] = set([santa, robot])

    s_turn = True

    for dir in dirs:
        if s_turn:
            santa = apply_dir(dir, santa)
            result.add(santa)
        else:
            robot = apply_dir(dir, robot)
            result.add(robot)
        s_turn = not s_turn

    return len(result)

# print(run_p2('^v'))
# print(run_p2('^>v<'))
# print(run_p2('^v^v^v^v^v'))

print(f'P2 answer: {run_p2(content)}')
# P2 answer: 2341