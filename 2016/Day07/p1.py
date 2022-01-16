from dataclasses import dataclass
from typing import TypeVar

Supernet = str
Hypernet = str

Network = Supernet | Hypernet

# def TODO(note: str | None = None) -> None:
#     todoMsg = 'This is still marked as TODO.'

#     if note:
#         raise Exception(f'{todoMsg} Note: {note}')
#     raise Exception(todoMsg)

@dataclass
class NetworkInfo:
    supernets: list[Supernet]
    hypernets: list[Hypernet]

def strToList(toListify: str) -> list[str]:
    return [x for x in toListify]

def parseLine(line: str) -> NetworkInfo:
    def subParse(data: NetworkInfo, blah: str) -> NetworkInfo:
        if ']' in blah:
            vals = blah.split(']')
            data.hypernets.append(vals[0])
            data.supernets.append(vals[1])
        else:
            data.supernets.append(blah)

        return data

    firstNet, *rest = line.split('[')
    result = NetworkInfo([firstNet], [])

    for x in rest:
        result = subParse(result, x)

    return result

def parseLines(rawLines: str) -> list[NetworkInfo]:
    return list(map(parseLine, rawLines.split('\n')))

A = TypeVar('A')

def windowed(lst: list[A], size: int) -> list[list[A]]:
    return [lst[i:i+size] for i in range(len(lst) - size + 1)]

def validateSupernet(supernet: Supernet) -> bool:
    if len(supernet) < 4:
        return False # too short to contain an ABBA

    for a,b,c,d in windowed(strToList(supernet), 4):
        if (a != b) & (b == c) & (a == d):
            return True # found ABBA, which means its a valid network
        else:
            pass # keep searching

    return False # done searching, no ABBA found

def validateHypernet(hypernet: Hypernet) -> bool:
    if len(hypernet) < 4:
        return True # too short to contain an ABBA

    for a,b,c,d in windowed(strToList(hypernet), 4):
        if (a != b) & (b == c) & (a == d):
            return False # hypernets are not valid if they contain an ABBA
        else:
            pass # keep searching

    return True # done searching, no ABBA found
    
def validate(info: NetworkInfo) -> bool:
    networkResult = False
    hypernetResult = True

    for supernet in info.supernets:
        networkResult |= validateSupernet(supernet)

    for hypernet in info.hypernets:
        hypernetResult &= validateHypernet(hypernet)

    return networkResult & hypernetResult

def run(rawList: str) -> int:
    info = parseLines(rawList)
    return len(list(filter(validate, info)))

testStr = """abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn
ektijwczwnlancuqfv[luqhtfgwmlilhwnk]gxgivxlnerdhbhetfz[bzczfdorrsptzikjmct]mfrsvxgxijtusmvjd[sbpnwycbrykuhsinudc]bmpikuskzlxcoidp"""

# print(list(map(validate, parseLines(testStr))))
# print(list(filter(validate, parseLines(testStr))))

f = open('2016\\Day07\\input.txt', 'r')
lines = f.read()
print(f'IPs that support TLS: {run(lines)}')
# IPs that support TLS: 110