from dataclasses import dataclass
from functools import reduce
from typing import TypeVar

Supernet = str
Hypernet = str

Network = Supernet | Hypernet

@dataclass
class NetworkInfo:
    supernets: list[Supernet]
    hypernets: list[Hypernet]
    ABAs: list[str]
    BABs: list[str]

def strToList(toListify: str) -> list[str]:
    return [x for x in toListify]

A = TypeVar('A')

def windowed(lst: list[A], size: int) -> list[list[A]]:
    return [lst[i:i+size] for i in range(len(lst) - size + 1)]

def parseLine(line: str) -> NetworkInfo:
    def populateABs(info: NetworkInfo) -> NetworkInfo:
        def extractNetwork(network: Network) -> list[str]:
            if len(network) < 3:
                return []

            results: list[str] = []
            
            for a,b,c in windowed(strToList(network), 3):
                if (a == c) & (a != b):
                    results.append(f'{a}{b}{c}')

            return results

        def extractReduce(nets: list[Network]) -> list[str]:
            def extractMerge(lst: list[str], net: Network) -> list[str]:
                lst.extend(extractNetwork(net))
                return lst

            def emptyList() -> list[str]:
                return []

            return reduce(lambda lst,net: extractMerge(lst,net), nets, emptyList())

        info.ABAs = extractReduce(info.supernets)
        info.BABs = extractReduce(info.hypernets)

        return info

    def subParse(data: NetworkInfo, blah: str) -> NetworkInfo:
        if ']' in blah:
            vals = blah.split(']')
            data.hypernets.append(vals[0])
            data.supernets.append(vals[1])
        else:
            data.supernets.append(blah)

        return data

    firstNet, *rest = line.split('[')
    result = NetworkInfo([firstNet], [], [], [])

    for x in rest:
        result = subParse(result, x)

    return populateABs(result)

def parseLines(rawLines: str) -> list[NetworkInfo]:
    return list(map(parseLine, rawLines.split('\n')))

def validate(info: NetworkInfo) -> bool:
    for a,b,c in info.ABAs:
        for x,y,z in info.BABs:
            if (b == x) & (b == z) & (c == y) & (a == y) & (a != x):
                return True

    return False

def run(rawLines: str) -> int:
    return len(list(filter(validate, parseLines(rawLines))))

testStr = """aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb"""

print(f'test result: {run(testStr)}')

f = open('2016\\Day07\\input.txt', 'r')
lines = f.read()
print(f'IPs that support TLS: {run(lines)}')
# IPs that support TLS: 242

