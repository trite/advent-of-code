import re
from dataclasses import dataclass

def count_chars(line: str) -> dict[str,int]:
    counts = {}

    for char in line:
        counts[char] = counts.get(char, 0) + 1
    
    return counts

@dataclass
class Room:
    value: str
    sectorId: int
    checksum: str
    actualChecksum: str

valueStr = 'value'
sectorStr = 'sectorId'
checksumStr = 'checksum'

def check_sum(roomValue: str) -> str:
    counts = count_chars(roomValue)
    sorted_counts = sorted(counts.items(), key=lambda x: (-x[1], x[0]))
    return ''.join([x[0] for x in sorted_counts[:5]])

def parse_line(line: str):
    pattern = re.compile(rf'(?P<{valueStr}>[a-z\-]+)-(?P<{sectorStr}>\d+)\[(?P<{checksumStr}>[a-z]+)\]')
    result = pattern.match(line)

    if result:
        roomValue = result.group(valueStr).replace('-', '')

        return Room(roomValue,
            int(result.group(sectorStr)),
            result.group(checksumStr),
            check_sum(roomValue))
    else:
        raise Exception(f'Could not parse line: {line}')


testList = """aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]"""

def run (rawList: str) -> int:
    lines = rawList.split('\n')

    return sum([
        x.sectorId
        for line in lines
        if (x := parse_line(line)).actualChecksum == x.checksum])

print(f'test data: {run(testList)}')

f = open('2016\\Day04\\input.txt', 'r')
lines = f.read()
result = run(lines)

print(f'actual result: {result}')