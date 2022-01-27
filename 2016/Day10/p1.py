from dataclasses import dataclass
from functools import partial
from typing import Tuple

# This problem isn't complicated enough to warrant all the extra type work I was doing

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

def parse_line(line: str) -> CmdValToBot | CmdBotToTarget:
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

def parse_lines(rawLines: str) -> Tuple[list[CmdValToBot], list[CmdBotToTarget], list[Bot]]:
    valCmds: list[CmdValToBot] = []
    botCmds: list[CmdBotToTarget] = []
    botList: list[Bot] = []

    for line in rawLines.split('\n'):
        match parse_line(line):
            case CmdValToBot() as cmd:
                valCmds.append(cmd)

            case CmdBotToTarget() as cmd:
                botCmds.append(cmd)
                botList.append(cmd.sourceBot)

            case other:
                raise Exception(f'Check your logic, this should only ever be a command! {other}')
    
    return (valCmds, botCmds, botList)

testStr = """value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2"""

# print(parse_lines(testStr))
"""
([
    CmdValToBot(value='5', targetBot=2),
    CmdValToBot(value='3', targetBot=1),
    CmdValToBot(value='2', targetBot=2)
],
[
    CmdBotToTarget(sourceBot=2, lowTarget=1, highTarget=0), 
    CmdBotToTarget(sourceBot=1, lowTarget=1, highTarget=0), 
    CmdBotToTarget(sourceBot=0, lowTarget=2, highTarget=0)
])
"""

# Next steps:
# while any bots in botCommands do not have 2 items in state:
#     loop through state:
#         if the length of the list is 2:
#             find that bot in botCommands, and give its values out appropriately

class State(dict[Bot, list[Value]]): pass

def is_bot_done(state: State, bot: Bot) -> bool:
    return len(state.get(bot, [])) == 2

def is_bot_still_going(state: State, bot: Bot) -> bool:
    return len(state.get(bot, [])) != 2

def any_left(state: State, bots: list[Bot]) -> bool:
    bot_check = partial(is_bot_still_going, state)
    return any(map(bot_check, bots))

def give_value_to_bot(state: State, val: Value, bot: Bot) -> State:
    current = state.get(bot, [])

    if val not in current:
        current.append(val)
        state[bot] = current
        if (val == Value('61')) | (val == Value('17')):
            print(f'Val {val} to Bot {bot} - {state[bot]}')

    if len(current) > 2:
        raise Exception(f'Value list for this bot exceeded 2! bot: {bot}, bot list: {current}, current val: {val}')

    # state[bot] = current # TODO: check if `current` is a reference in this case, this line might be unnecessary
    return state

def give_value_to_target(state: State, target: Target, val: Value) -> State:
    match target:
        case Bot() as bot:
            return give_value_to_bot(state, val, bot)
        case Output(): # as output:
            # Do nothing for now, probably needed in part 2?
            return state

# def run_iter(state: State, cmds: list[CmdBotToTarget]) -> State:
def run_iter(cmds: list[CmdBotToTarget], state: State) -> State:
    def run_cmd(state: State, cmd: CmdBotToTarget) -> State:
        if is_bot_done(state, cmd.sourceBot):
            state = give_value_to_target(state, cmd.lowTarget,  min(state[cmd.sourceBot]))
            state = give_value_to_target(state, cmd.highTarget, max(state[cmd.sourceBot]))
        return state

    for cmd in cmds:
        run_cmd(state, cmd)

    return state
    
def apply_vals(state: State, cmds: list[CmdValToBot]) -> State:
    for cmd in cmds:
        state = give_value_to_bot(state, cmd.value, cmd.targetBot)

    return state






# Steps now:
# apply all `valCmds` to state first
# then run_iter on state using botCmds and make sure things look right with the test data

"""
valCmds, botCmds, botList = parse_lines(testStr)

state = apply_vals(State({}), valCmds)

print(state)
print('=========')

iter = partial(run_iter, botCmds)

# print(iter(state))
# {2: ['5', '2'], 1: ['3']}
# =========
# {2: ['5', '2'], 1: ['3', '2'], 0: ['5', '3']}
"""


with open('2016\\Day10\\input.txt', 'r') as f:
    fileContent = f.read()

valCmds, botCmds, botList = parse_lines(fileContent)

state = apply_vals(State({}), valCmds)

print(state)

iter = partial(run_iter, botCmds)

while any_left(state, botList):
    state = iter(state)

print(state)
# print(dict(filter(lambda vals: (Value(61) in vals) & (Value(17) in vals),
#     state.items())))

for bot, vals in state.items():
    if (Value(61) in vals) & (Value(17) in vals):
        print(f'Answer: {bot} ({vals})')

# Wrong answer apparently...
# Answer: 83 (['61', '17'])

# Enter interactive mode for some poking around:
# import code
# code.interact(local=locals())


# Val 17 to Bot 4 - ['53', '17']
# Val 17 to Bot 171 - ['31', '17']
# Val 17 to Bot 150 - ['37', '17']
# Val 17 to Bot 25 - ['29', '17']
# Val 17 to Bot 92 - ['2', '17']
# Val 17 to Bot 142 - ['47', '17'] <== 142 should've receved 2, its comparing strings, duh


# actual answer this time
# Answer: 118 ([61, 17])