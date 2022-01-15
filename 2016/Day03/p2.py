from typing import TypeVar

T = TypeVar('T')

testList = """ 810  679   10
  783  255  616
  545  626  626
   84  910  149
  607  425  901
  556  616  883"""

def split_list(toSplit: list[T], count: int) -> list[list[T]]:
    blah = [toSplit[i:i+count] for i in range(0, len(toSplit), count)]
    return blah

def parse_line(line: str) -> list[int]:
    result = [int(x) for x in line.split()]
    return result

def transpose(toTranspose: list[list[int]]) -> list[list[int]]:
    blah: map[list[int]] = map(list, zip(*toTranspose))
    return list(blah)

# print(transpose([[1,2,3], [11,22,33], [111,222,333]]))

def run(rawList: str) -> int:
    total = 0
    lines = rawList.split('\n')
    stuff = [parse_line(line) for line in lines]
    groups = split_list(stuff, 3)
    for group in groups:
        newGroup = transpose(group)
        for triangle in newGroup:
            triangle.sort()
            if triangle[0] + triangle[1] > triangle[2]:
                total += 1
    
    return total

print(f'test result: {run(testList)}')

f = open('2016\\Day03\\input.txt', 'r')
lines = f.read()
print(f'actual result: {run(lines)}')

# test result: 5
# actual result: 1544