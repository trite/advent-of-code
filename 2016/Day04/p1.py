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

def parse_line(line: str):
    pattern = re.compile(r'(?P<value>[a-z\-]+)-(?P<sectorId>\d+)\[(?P<checksum>[a-z]+)\]')
    result = pattern.match(line)
    if result:
        return Room(result.group('value'), int(result.group('sectorId')), result.group('checksum'))
    else:
        raise Exception(f'Could not parse line: {line}')

print(parse_line("aaaaa-bbb-z-y-x-123[abxyz]"))