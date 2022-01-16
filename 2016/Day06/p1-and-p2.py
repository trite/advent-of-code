from typing import TypeVar

A = TypeVar('A')

def transpose(toTranspose: list[list[A]]) -> list[list[A]]:
    result: map[list[A]] = map(list, zip(*toTranspose))
    return list(result)

def parse(toParse: str) -> list[list[str]]:
    return transpose([[y for y in x] for x in toParse.split('\n')])

def most_frequent(lst: list[str]) -> str:
    return max(set(lst), key = lst.count)

def run(toRun: str) -> str:
    parsed = parse(toRun)
    return ''.join([most_frequent(x) for x in parsed])

testStr = """eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar"""

print(f'test result: {run(testStr)}')

f = open('2016\\Day06\\input.txt', 'r')
lines = f.read()
print(f'actual result: {run(lines)}')

def least_frequent(lst: list[str]) -> str:
    return min(set(lst), key = lst.count)

def run_p2(toRun: str) -> str:
    parsed = parse(toRun)
    return ''.join([least_frequent(x) for x in parsed])

print(f'test result p2: {run_p2(testStr)}')
print(f'actual result p2: {run_p2(lines)}')