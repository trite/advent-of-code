testVals = {
    '(())': '0',
    '()()': '0',
    '(((': '3',
    '(()(()(': '3',
    '))(((((': '3',
    '())': '-1',
    '))(': '-1',
    ')))': '-3',
    ')())())': '-3'
}

def apply_char(char: str, floor: int) -> int:
    match char:
        case '(':
            return floor + 1
        case ')':
            return floor - 1
        case other:
            raise Exception(f'Invalid input, expected open or close parenthesis, received: "{other}"')

def run_p1(toRun: str) -> int:
    floor = 0

    for c in toRun:
        floor = apply_char(c, floor)
    
    return floor

# Try all the test values
# for toTest, expected in testVals.items():
#     actual = run_p1(toTest)
#     print(f'Result: {expected == str(actual)}, Expected: {expected}, Actual: {actual}')

with open('2015\\Day01\\input.txt', 'r') as f:
    content = f.read()

print(f'P1 answer: {run_p1(content)}')
# P1 answer: 232

def run_p2(toRun: str) -> int:
    floor = 0
    position = 0
    
    for c in toRun:
        position += 1
        floor = apply_char(c, floor)
        
        if floor == -1:
            return position

    raise Exception("Shouldn't have reached this point!")

print(f'P2 answer: {run_p2(content)}')
# P2 answer: 1783