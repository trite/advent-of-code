import hashlib

def md5(toHash: str) -> str:
    return hashlib.md5(toHash.encode()).hexdigest()

def check(toCheck: str) -> bool:
    return md5(toCheck).startswith('00000')

def crack(door: str) -> str:
    Result = dict[str,str]

    result: Result = {
        '0': '_',
        '1': '_',
        '2': '_',
        '3': '_',
        '4': '_',
        '5': '_',
        '6': '_',
        '7': '_'
    }

    def toString(result: Result) -> str:
        return ''.join(result.values())

    def notDone(result: Result) -> bool:
        return '_' in toString(result)

    i = 0
    running = True
    while running:
        i += 1
        current = f'{door}{i}'
        if check(current):
            hash = md5(current)
            pos = hash[5]
            val = str(hash[6])
            if result.get(pos, 'x') == '_':
                result[pos] = val
                running = notDone(result)
                print(f'New character found!: {toString(result)}')

    return toString(result)

testResult = crack('abc')
print(f'test result: {testResult}')

actualResult = crack('abbhdwsy')
print(f'actual result: {actualResult}')

"""
New character found!: _5______
New character found!: _5__e___
New character found!: _5__e__3
New character found!: _5_ce__3
New character found!: 05_ce__3
New character found!: 05_ce_e3
New character found!: 05_ce8e3
New character found!: 05ace8e3
test result: 05ace8e3

New character found!: 4_______
New character found!: 42______
New character found!: 42___1__
New character found!: 42___19_
New character found!: 42___197
New character found!: 424__197
New character found!: 424_0197
New character found!: 424a0197
actual result: 424a0197
"""