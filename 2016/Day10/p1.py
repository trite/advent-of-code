from dataclasses import dataclass

class Value(int): pass
class Bot(int): pass
class Output(int): pass

Target = Bot | Output

@dataclass
class CmdValToBot:
    value: Value
    targetBot: Bot

    def __init__(self, v: str, t: str) -> None:
        self.value = Value(int(v))
        self.targetBot = Bot(int(t))

@dataclass
class CmdBotToTarget:
    sourceBot: Bot
    lowTarget: Target
    highTarget: Target

Command = CmdValToBot | CmdBotToTarget

def parse_line(line: str) -> Command:
    def parse_bot_target(targetType: str, targetValue: str) -> Target:
        match targetType, targetValue:
            case 'output', x:
                return Output(int(x))
            
            case 'bot', x:
                return Bot(int(x))

        raise Exception(f"Expected this to be 'output' or 'bot': {targetType}")

    match line.split(' '):
        case ['value', value, 'goes', 'to', 'bot', targetBot]:
            return CmdValToBot(value, targetBot)

        case ['bot', sourceBot,
                'gives', 'low', 'to', lowType, lowTarget,
                'and', 'high', 'to', highType, highTarget]:
            return CmdBotToTarget(Bot(int(sourceBot)),
                parse_bot_target(lowType, lowTarget),
                parse_bot_target(highType, highTarget))

    raise Exception(f"Couldn't parse this one for some reason: {line}")

testStr = """value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2"""

# print(list(map(parse_line, testStr.split('\n'))))

"""
[CmdValToBot(value=5, targetBot=2),
 CmdBotToTarget(sourceBot=2, lowTarget=1, highTarget=0),
 CmdValToBot(value=3, targetBot=1),
 CmdBotToTarget(sourceBot=1, lowTarget=1, highTarget=0),
 CmdBotToTarget(sourceBot=0, lowTarget=2, highTarget=0),
 CmdValToBot(value=2, targetBot=2)]
"""

class State(dict[Bot, list[Value]]): pass

def initial_state() -> State:
    return State({
        Bot(1): [Value(3)],
        Bot(2): [Value(2)]
    })

def give_value_to_bot(state: State, bot: Bot, value: Value) -> State:
    match state.get(bot, []):
        case [] | [_] as lst:
            lst.append(value)

        case ohNo:
            raise Exception(f"Expected an empty or single-item list, but found: {ohNo}")
    
    return state

def give_value_to_output(state: State, output: Output, value: Value) -> State:
    """Leaving this here in case it is used in p2, for now it just eats values. Om nom nom."""
    return state

def give_value_to_target(state: State, target: Target, value: Value) -> State:
    match target:
        case Bot() as bot:
            return give_value_to_bot(state, bot, value)
            
        case Output() as output:
            return give_value_to_output(state, output, value)

        case other:
            raise Exception(f"Target was an unexpected type! {other}")

def from_bot_to_target(state: State, source: Bot, low: Target, high: Target) -> State:
    # fromBot = state.get(source, [])
    match state.get(source, []):
        case [_, _] as lst:
            state = give_value_to_target(state, low, min(lst))
            state = give_value_to_target(state, high, max(lst))
            state[source] = []

        case ohNo:
            raise Exception(f"Expected a list with 2 items, but found: {ohNo}")

    return state

def apply_command(state: State, command: Command) -> State:
    match command:
        case CmdValToBot(value, target):
            return give_value_to_target(state, target, value)

        case CmdBotToTarget(source, low, high):
            return from_bot_to_target(state, source, low, high)

        case other:
            raise Exception(f"Unexpected command type!: {other}")
