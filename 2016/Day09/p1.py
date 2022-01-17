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

    head, _, rest = stream.partition(')')
    pre, _, marker = head.partition('(')
    length, count = marker.split('x')

    result = int(length) * int(count) + len(pre)
    newStream = Stream(rest[int(length):])

    return result, newStream

def run(rawInput: str) -> int:
    stream = Stream(rawInput)
    result = 0

    while len(stream) > 0:
        x, stream = pop_next(stream)
        result += x

    return result


print(run('ADVENT'))
print(run('A(1x5)BC'))
print(run('(3x3)XYZ'))
print(run('A(2x2)BCD(2x2)EFG'))
print(run('(6x1)(1x3)A'))
print(run('X(8x2)(3x3)ABCY'))


f = open('2016\\Day09\\input.txt', 'r')
content = f.read()
print(run(content))

# answer: 120765