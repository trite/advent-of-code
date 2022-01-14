testList = """ 5 10 25
  810  679   10
  783  255  616
  545  626  626
   84  910  149"""

def run(rawList: str) -> list:
    lines = rawList.split('\n')

    def parse_line(line: str) -> list:
        sides = [int(x) for x in line.split()]
        sides.sort()
        if sides[0] + sides[1] > sides[2]:
            return 1
        else:
            return 0

    return sum([parse_line(line) for line in lines])
print(run(testList))

test = [2,7,5,8,3,9]
test.sort()
print(f'test result: {run(testList)}')

f = open('2016\\Day03\\input.txt', 'r')
lines = f.read()
print(f'actual result: {run(lines)}')