import hashlib, itertools

def md5(s: str) -> str:
    return hashlib.md5(s.encode()).hexdigest()

def solve_p1(key: str) -> int:
    # result = 1

    # while True:
    #     if md5(key + str(result)).startswith('00000'):
    #         break
    #     result += 1

    for x in itertools.count(start=1):
        if md5(key + str(x)).startswith('00000'):
            return x

    raise Exception(f'This should be unreachable...')

# print(solve_p1('abcdef'))

content = 'ckczppom'

print(f'P1 answer: {solve_p1(content)}')
# P1 answer: 117946