# TODO: Need to spend some more time with Python module pathing. This feels gross
from os.path import dirname, realpath
import sys
sys.path.append(dirname(dirname(dirname(realpath(__file__)))))
from trite_lib import common

def str_to_list(s: str) -> list[str]:
    return [x for x in s]

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

p1 = len(list(filter(validate_p1, content.split('\n'))))

print(f'P1 answer: {p1}')
# P1 answer: 258

def validate_p2_pairs(toValidate: str) -> bool:
    # def pop(s: str):
    #     return 
    
    return common.TODO("Stuff")

def validate_p2_trips(toValidate: str) -> bool:
    segments = common.windowed(str_to_list(toValidate), 3)
    return any(a == c for a,_,c in segments)

print(validate_p2_pairs('blah'))