# TODO: Need to spend some more time with Python module pathing. This feels gross
from os.path import dirname, realpath
import sys
sys.path.append(dirname(dirname(dirname(realpath(__file__)))))
from trite_lib import common

from typing import Callable

def str_to_list(s: str) -> list[str]:
    return [x for x in s]

def list_to_str(lst: list[str]) -> str:
    return ''.join(lst)

def validate_p1_vowels(toValidate: str) -> bool:
    def vowel(char: str) -> bool:
        match char:
            case 'a' | 'e' | 'i' | 'o' | 'u':
                return True
            case _:
                return False

    return len(list(filter(vowel, toValidate))) >= 3

def validate_p1_double(toValidate: str) -> bool:
    segments = common.windowed(str_to_list(toValidate), 2)
    return any(x == y for x,y in segments)

def validate_p1_special(toValidate: str) -> bool:
    return not any(x in toValidate for x in ['ab', 'cd', 'pq', 'xy'])

def validate_p1(toValidate: str) -> bool:
    return all([
        validate_p1_vowels(toValidate),
        validate_p1_double(toValidate),
        validate_p1_special(toValidate)
    ])

with open('2015\\Day05\\input.txt', 'r') as f:
    content = f.read()

def solve(validator: Callable[[str], bool], toValidate: str) -> int:
    return len(list(filter(validator, toValidate.split('\n'))))

print(f'P1 answer: {solve(validate_p1, content)}')
# P1 answer: 258

def validate_p2_pairs(toValidate: str) -> bool:
    while len(toValidate) > 3:
        headList, restList = common.split_list(str_to_list(toValidate), 2)
        head, rest = list_to_str(headList), list_to_str(restList)
        
        if head in rest:
            return True

        toValidate = head[1] + rest

    return False

def validate_p2_trips(toValidate: str) -> bool:
    segments = common.windowed(str_to_list(toValidate), 3)
    return any(a == c for a,_,c in segments)

def validate_p2(toValidate: str) -> bool:
    return all([
        validate_p2_pairs(toValidate),
        validate_p2_trips(toValidate)
    ])

print(f'P2 answer: {solve(validate_p2, content)}')
# P2 answer: 53