from typing import Callable, Tuple

def parse(s: str) -> Tuple[int,int,int]:
    result = list(map(int, s.split('x')))

    if len(result) == 3:
        result.sort()
        a,b,c = result
        return a,b,c

    raise Exception(f'Failed to parse {s}')

def calc_paper(box: str) -> int:
    a,b,c = parse(box)
    return 3*a*b + 2*b*c + 2*c*a

def solve(func: Callable[[str], int], boxes: str) -> int:
    return sum(list(map(func, boxes.split('\n'))))

with open('2015\\Day02\\input.txt', 'r') as f:
    content = f.read()

print(f'P1 answer: {solve(calc_paper, content)}')
# P1 answer: 1606483

def calc_ribbon(box: str) -> int:
    a,b,c = parse(box)
    return 2*a + 2*b + a*b*c

print(f'P2 answer: {solve(calc_ribbon, content)}')
# P2 answer: 3842356