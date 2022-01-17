from dataclasses import dataclass

@dataclass
class Marker:
    length: int
    count: int

class Stream(str):
    pass

# return character count the removed chunk represents and remaining chars
def pop_next(stream: Stream) -> tuple[int, Stream]:
    if ')' not in stream:
        return len(stream), Stream('')

    if len(stream) == 0:
        raise Exception('This should not happen but could cause an infinite loop if it does')

    head, _, tail = stream.partition(')')
    pre, _, marker = head.partition('(')
    length, count = map(int, marker.split('x'))

    part2 = run(tail[:length])

    result = len(pre) + (part2 * count)
    newStream = Stream(tail[length:])

    return result, newStream

def run(rawInput: str) -> int:
    stream = Stream(rawInput)
    result = 0

    while len(stream) > 0:
        x, stream = pop_next(stream)
        result += x

    return result


# print(run('(3x3)XYZ'))
# print(run('X(8x2)(3x3)ABCY'))
# print(run('(27x12)(20x12)(13x14)(7x10)(1x12)A'))
# print(run('(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN'))


f = open('2016\\Day09\\input.txt', 'r')
content = f.read()
print(run(content))

# answer: 11658395076