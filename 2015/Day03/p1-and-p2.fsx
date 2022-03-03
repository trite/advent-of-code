type Position = int * int

type StateP1 =
    {
        CurrentPosition : Position
        History : Set<Position>
    }

let p1TestData =
    Map [
        (">", 2)
        ("^>v<", 4)
        ("^v^v^v^v^v", 2)
    ]

let defaultState =
    {
        CurrentPosition = (0, 0)
        History = Set.ofList [(0, 0)]
    }

let applyCommand ((x,y) : Position) (c : char) : Position =
    match c with
    | '^' -> (x+1, y)
    | 'v' -> (x-1, y)
    | '<' -> (x, y-1)
    | '>' -> (x, y+1)
    | err -> failwith $"Expected one of: ['^', '<', 'v', '>'] but received: {err}"

let applyCommandToState (state : StateP1) (c : char) : StateP1 =
    let newPosition = applyCommand state.CurrentPosition c

    {
        CurrentPosition = newPosition
        History = state.History.Add(newPosition)
    }

let p1Commands (commandsStr : string) : int =
    let finalState = 
        commandsStr.ToCharArray()
        |> Array.fold applyCommandToState defaultState

    finalState.History.Count

p1TestData
|> Map.iter (
    fun k v ->
        let result = p1Commands k
        if result <> v then
            printfn $"[{k}] was expected to produce {result} but instead produced {v}!"
)

let runCommands (toRun : string -> int) : int =
    System.IO.File.ReadAllText("2015\\Day03\\input.txt")
    |> toRun

runCommands p1Commands
|> printfn "Part 1 answer: %A"
// Part 1 answer: 2081

let p2TestData =
    Map [
        ("^v", 3)
        ("^>v<", 3)
        ("^v^v^v^v^v", 11)
    ]

type StateP2 =
    {
        Santa : Position
        RoboSanta : Position
        History : Set<Position>
    }

let p2Commands (commandsStr : string) : int =
    let applyCommandsToState (state : StateP2) (santaCommand : char) (roboSantaCommand : char) : StateP2 =
        let newSanta = applyCommand state.Santa santaCommand
        let newRoboSanta = applyCommand state.RoboSanta roboSantaCommand
        {
            Santa = newSanta
            RoboSanta = newRoboSanta
            History = state.History.Add(newSanta).Add(newRoboSanta)
        }

    let rec runCommands' (commands : char list) (state : StateP2) : StateP2 =
        match commands with
        | [] -> state
        | santa::roboSanta::rest -> runCommands' rest (applyCommandsToState state santa roboSanta)
        | _  -> failwith $"This shouldn't have happened, check your logic!"

    let runCommands (commands : char list) : StateP2 =
        let state = {
            Santa = (0,0)
            RoboSanta = (0,0)
            History = Set.ofList [(0,0)]
        }

        runCommands' commands state

    let result = 
        commandsStr.ToCharArray()
        |> Array.toList
        |> runCommands

    result.History.Count

p2TestData
|> Map.iter (
    fun k v ->
        let result = p2Commands k
        if result <> v then
            printfn $"[{k}] was expected to produce {result} but instead produced {v}!"
)

runCommands p2Commands
|> printfn "Part 2 answer: %A"
// Part 2 answer: 2341

