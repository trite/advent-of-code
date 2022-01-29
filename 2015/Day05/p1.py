from typing import TypeVar


def validate_vowels(toValidate: str) -> bool:
    def vowel(char: str) -> bool:
        match char:
            case 'a' | 'e' | 'i' | 'o' | 'u':
                return True
            case _:
                return False

    return len(list(filter(vowel, toValidate))) >= 3

# print(validate_vowels('aei'))
# print(validate_vowels('xazegov'))
# print(validate_vowels('aeiaeiouaeiouaeiou'))

A = TypeVar('A')

def windowed(lst: list[A], size: int) -> list[list[A]]:
    return [lst[i:i+size] for i in range(len(lst) - size + 1)]

def validate_double(toValidate: str) -> bool:
    blah = windowed([x for x in toValidate], 2)
    for x,y in blah:
        if x == y:
            return True

    return False

# print(validate_double('abcdde'))
# print(validate_double('aabbccdd'))

def validate_special(toValidate: str) -> bool:
    for x in ['ab', 'cd', 'pq', 'xy']:
        if x in toValidate:
            return False

    return True

def validate(toValidate: str) -> bool:
    return all([
        validate_vowels(toValidate),
        validate_double(toValidate),
        validate_special(toValidate)
    ])

# print(validate('ugknbfddgicrmopn'))
# print(validate('aaa'))
# print(validate('jchzalrnumimnmhp'))
# print(validate('haegwjzuvuyypxyu'))
# print(validate('dvszwmarrgswjxmb'))

with open('2015\\Day05\\input.txt', 'r') as f:
    content = f.read()

p1 = len(list(filter(validate, content.split('\n'))))

print(f'P1 answer: {p1}')
# P1 answer: 258
