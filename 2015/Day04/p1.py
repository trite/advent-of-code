import hashlib, itertools

def md5(s: str) -> str:
    return hashlib.md5(s.encode()).hexdigest()

def solve(key: str, zeroCount: int) -> int:
    zeroes = ''.join(['0' for _ in range(zeroCount)])

    for x in itertools.count(start=1):
        if md5(key + str(x)).startswith(zeroes):
            return x

    raise Exception('This should be unreachable...')

content = 'ckczppom'

print(f'P1 answer: {solve(content, 5)}')
# P1 answer: 117946

print(f'P1 answer: {solve(content, 6)}')
# P1 answer: 3938038