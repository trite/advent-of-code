# print(ord('a')) # 97
# print(ord('z')) # 122

def shift_char(char: str, shift: int) -> str:
    match char:
        case '-':
            return ' '
        case x if ord(x) >= 97 & ord(x) <= 122:

            return 'fill this out later'
        case _:
            return 'oh noes!'