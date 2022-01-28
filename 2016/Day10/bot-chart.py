from dataclasses import dataclass
from functools import partial
from typing import Tuple

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


@dataclass
class State:
    bots: dict[Bot, list[Value]]
    outputs: dict[Output, Value]

def is_bot_done(state: State, bot: Bot) -> bool:
    return len(state.bots.get(bot, [])) == 2

def is_bot_still_going(state: State, bot: Bot) -> bool:
    return len(state.bots.get(bot, [])) != 2

def any_left(state: State, bots: list[Bot]) -> bool:
    bot_check = partial(is_bot_still_going, state)
    return any(map(bot_check, bots))

def give_value_to_bot(state: State, val: Value, bot: Bot) -> State:
    current = state.bots.get(bot, [])

    if val not in current:
        current.append(val)
        state.bots[bot] = current
        # if (val == Value('61')) | (val == Value('17')):
        #     print(f'Val {val} to Bot {bot} - {state.bots[bot]}')

    if len(current) > 2:
        raise Exception(f'Value list for this bot exceeded 2! bot: {bot}, bot list: {current}, current val: {val}')

    return state

def give_value_to_output(state: State, val: Value, output: Output) -> State:
    current = state.outputs.get(output, None)

    if (current != None) & (current != val):
        raise Exception(f'Output {output} currently has value: {state.outputs[output]}, trying to add {val}')

    state.outputs[output] = val

    return state

def give_value_to_target(state: State, target: Target, val: Value) -> State:
    match target:
        case Bot() as bot:
            return give_value_to_bot(state, val, bot)
        case Output() as output:
            return give_value_to_output(state, val, output)

def run_iter(cmds: list[CmdBotToTarget], state: State) -> State:
    def run_cmd(state: State, cmd: CmdBotToTarget) -> State:
        if is_bot_done(state, cmd.sourceBot):
            state = give_value_to_target(state, cmd.lowTarget,  min(state.bots[cmd.sourceBot]))
            state = give_value_to_target(state, cmd.highTarget, max(state.bots[cmd.sourceBot]))
        return state

    for cmd in cmds:
        run_cmd(state, cmd)

    return state
    
def apply_vals(state: State, cmds: list[CmdValToBot]) -> State:
    for cmd in cmds:
        state = give_value_to_bot(state, cmd.value, cmd.targetBot)

    return state


# with open('2016\\Day10\\input.txt', 'r') as f:
#     fileContent = f.read()

# Test content
fileContent = """value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2"""

valCmds, botCmds, botList = parse_lines(fileContent)
state = apply_vals(State({}, {}), valCmds)

# print(state)

while any_left(state, botList):
    state = run_iter(botCmds, state)

print(state)

for cmd in valCmds:
    val = cmd.value
    bot = cmd.targetBot

    print(f"    in{val}(Value {val}) -- {val} --> bot{bot}{{Bot {bot}}}")


def fmt(target: Target):
    match target:
        case Bot() as x:
            return f"bot{x}{{Bot {x}}}"

        case Output() as x:
            return f"output{x}(Output {x})"

for cmd in botCmds:
    src = fmt(cmd.sourceBot)
    hiTar = fmt(cmd.highTarget)
    loTar = fmt(cmd.lowTarget)

    vals = state.bots[cmd.sourceBot]
    loVal = min(vals)
    hiVal = max(vals)


    print(f"    {src} -- {hiVal} --> {hiTar}")
    print(f"    {src} -- {loVal} --> {loTar}")