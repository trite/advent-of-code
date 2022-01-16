def md5(toHash: str) -> str:
    import hashlib
    return hashlib.md5(toHash.encode()).hexdigest()

def check(toCheck: str) -> bool:
    return md5(toCheck).startswith('00000')

def crack(door: str) -> str:
    result = ''
    i = 0
    while len(result) < 8:
        i += 1
        current = f'{door}{i}'
        if check(current):
            print(f'found: {current} ({md5(current)})')
            # result += str(i)[0]
            result += md5(current)[5]

    return result


# print(md5('abc3231929'))
# print(check('abc3231929'))
# print(check('abc3231928'))

testResult = crack('abc')
print(f'test result: {testResult}')

actualResult = crack('abbhdwsy')
print(f'actual result: {actualResult}')

"""
found: abc3231929 (00000155f8105dff7f56ee10fa9b9abd)
found: abc5017308 (000008f82c5b3924a1ecbebf60344e00)
found: abc5278568 (00000f9a2c309875e05c5a5d09f1b8c4)
found: abc5357525 (000004e597bd77c5cd2133e9d885fe7e)
found: abc5708769 (0000073848c9ff7a27ca2e942ac10a4c)
found: abc6082117 (00000a9c311683dbbf122e9611a1c2d4)
found: abc8036669 (000003c75169d14fdb31ec1593915cff)
found: abc8605828 (0000000ea49fd3fc1b2f10e02d98ee96)
test result: 18f47a30

found: abbhdwsy1739529 (000008bfb72caf77542c32b53a73439b)
found: abbhdwsy1910966 (0000004ed0ede071d293b5f33de2dc2f)
found: abbhdwsy1997199 (0000012be6057b2554c26bfddab18b08)
found: abbhdwsy2854555 (00000bf3f1ca8d1f229aa50b3093b2be)
found: abbhdwsy2963716 (00000512874cc40b764728993dd71ffb)
found: abbhdwsy3237361 (0000069710beec5f9a1943a610be52d8)
found: abbhdwsy4063427 (00000a8da36ee9b7e193f956cf701911)
found: abbhdwsy7777889 (00000776b6ff41a7e30ed2d4b6663351)
actual result: 801b56a7
"""