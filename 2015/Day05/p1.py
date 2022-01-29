from typing import TypeVar

def validate_vowels(toValidate: str) -> bool:
    def vowel(char: str) -> bool:
        match char:
            case 'a' | 'e' | 'i' | 'o' | 'u':
                return True
            case _:
                return False

    return len(list(filter(vowel, toValidate))) >= 3

A = TypeVar('A')

def windowed(lst: list[A], size: int) -> list[list[A]]:
    return [lst[i:i+size] for i in range(len(lst) - size + 1)]

def validate_double(toValidate: str) -> bool:
    segments = windowed([x for x in toValidate], 2)
    return any([x == y for x,y in segments])

def validate_special(toValidate: str) -> bool:
    return not any([x in toValidate for x in ['ab', 'cd', 'pq', 'xy']])

def validate_p1(toValidate: str) -> bool:
    return all([
        validate_vowels(toValidate),
        validate_double(toValidate),
        validate_special(toValidate)
    ])

with open('2015\\Day05\\input.txt', 'r') as f:
    content = f.read()

p1 = len(list(filter(validate_p1, content.split('\n'))))

print(f'P1 answer: {p1}')
# P1 answer: 258