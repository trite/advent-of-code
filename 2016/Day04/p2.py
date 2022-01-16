import re

from dataclasses import dataclass
from functools import partial

class BogusRoom:
    pass

@dataclass
class DecryptedRoom:
    value: str
    sectorId: int

@dataclass
class EncryptedRoom:
    value: str
    sectorId: int
    checksum: str
    actualChecksum: str

Room = EncryptedRoom | DecryptedRoom | BogusRoom

def count_chars(line: str) -> dict[str,int]:
    counts: dict[str,int] = {}

    for char in line:
        counts[char] = counts.get(char, 0) + 1
    
    return counts

def check_sum(roomValue: str) -> str:
    counts = count_chars(roomValue.replace('-',''))
    sorted_counts = sorted(counts.items(), key=lambda x: (-x[1], x[0]))
    return ''.join([x[0] for x in sorted_counts[:5]])

valueStr = 'value'
sectorStr = 'sectorId'
checksumStr = 'checksum'

def parse_line(line: str) -> Room:
    pattern = re.compile(rf'(?P<{valueStr}>[a-z\-]+)-(?P<{sectorStr}>\d+)\[(?P<{checksumStr}>[a-z]+)\]')
    result = pattern.match(line)

    if result:
        roomValue = result.group(valueStr)
        room = EncryptedRoom(roomValue,
            int(result.group(sectorStr)),
            result.group(checksumStr),
            check_sum(roomValue))

        if room.actualChecksum == room.checksum:
            return room
        else:
            return BogusRoom()
    else:
        raise Exception(f'Could not parse line: {line}')

def shift_char(shift: int, char: str) -> str:
    def actual_shift(char: str, shift: int) -> str:
        actual = (shift % 26)
        newChar = ord(char) + actual
        if newChar > 122:
            newChar -= 26
        return chr(newChar)

    match char.lower():
        case '-':
            return ' '
        case x if ord(x) >= 97 & ord(x) <= 122:
            return actual_shift(x, shift)
        case _:
            raise Exception("Missed something, this shouldn't happen!")

def shift_room(room: Room) -> Room: 
    match room:
        case EncryptedRoom(value, sectorId, _, _):
            shift = partial(shift_char, sectorId)
            shiftedValue = ''.join(map(shift, value))
            return DecryptedRoom(shiftedValue, sectorId)
        case x:
            return x

def print_room(room: Room) -> None:
    match room:
        case DecryptedRoom(value, sectorId) if value.find('north') != -1:
            print(f'{sectorId} {value}')
        case _:
            pass

def run(rawList: str) -> None:
    lines = rawList.split('\n')

    for line in lines:
        print_room(shift_room(parse_line(line)))

f = open('2016\\Day04\\input.txt', 'r')
lines = f.read()
result = run(lines)

# 482 northpole object storage
# answer: 482