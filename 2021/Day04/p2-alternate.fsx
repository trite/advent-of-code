let testInput = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

type Callouts =
    {
        Done : int list
        Left : int list
    }

type Cell =
    | Uncalled of int
    | Called of int

type Row = Cell list

type Board =
    | Finished of score : int
    | Unfinished of Row list

type Boards = Board list

type GameState =
    {
        Callouts : Callouts
        Boards : Boards
    }

let split (c : char) (str : string) =
    str.Split c
    |> Array.toList

let splitOn (split : string) (lst : string list) =
    let rec splitOn' accList acc split (lst : string list) =
        match lst with
        | [] ->
            // TODO: Is there a way to handle this without reversing lists so frequently?
            (List.rev acc)::accList
            |> List.rev
        | x::xs ->
            if split = x then
                splitOn' (List.rev acc::accList) [] split xs
            else
                splitOn' accList (x::acc) split xs
    splitOn' [] [] split lst

let parse (lst : string list) =
    splitOn "" lst
    |> List.splitAt 1
    |> fun (cRaw,bRaw) ->
        let callouts =
            {
                Done = []
                Left =             
                    cRaw
                    |> List.head
                    |> List.head
                    |> split ','
                    |> List.map int
            }

        let boards =
            bRaw
            |> List.map (
                List.map (
                    split ' '
                    >> List.filter (fun x -> x <> "")
                    >> List.map (fun x -> x |> int |> Uncalled)
                    // >> List.map (fun x -> (int x, false))
                )
                >> Unfinished
            )
        
        {
            Callouts = callouts
            Boards = boards
        }

let testGame = 
    testInput
    |> split '\n'
    |> parse

let printGame (game : GameState) =
    let previousCallouts (game : GameState) =
        match game.Callouts.Done with
        | [] -> "No callouts yet"
        | [x] -> $"{x}"
        | x::xs -> $"{x} => {xs}"

    let printBoards (boards : Boards) =
        let printCell (cell : Cell) =
            match cell with
            | Uncalled x -> $"[_{x,2}]"
            | Called   x -> $"[X{x,2}]"

        let printBoard (board : Board) =
            match board with
            | Finished score ->
                [ "|                           |";
                  "|                           |";
                 $"|     Done. Score: {score,-9}|";
                  "|                           |";
                  "|                           |"]
            | Unfinished rows ->
                rows
                |> List.map (
                    List.map printCell
                    >> String.concat " "
                )

        boards
        |> List.map printBoard
        |> List.transpose
        |> List.map (String.concat "     ")
        |> fun x -> "\n" + (String.concat "\n" x)
    
    $"""
    Previous callouts : {previousCallouts game}
    Remaining callouts: {game.Callouts.Left}

    Boards: {printBoards game.Boards}
    """
    |> printfn "%s"

printGame testGame

// Testing the printGame function for formatting, only planning to use this for the example for now so 3 boards should be the limit...
printGame {
    Callouts = {
        Done = [1..10]
        Left = [11..20]
    }
    Boards = [
        Unfinished
            [[Uncalled 22; Uncalled 13; Uncalled 17; Uncalled 11; Uncalled 0];
             [Uncalled 8; Uncalled 2; Uncalled 23; Uncalled 4; Uncalled 24];
             [Uncalled 21; Uncalled 9; Uncalled 14; Uncalled 16; Uncalled 7];
             [Uncalled 6; Uncalled 10; Uncalled 3; Uncalled 18; Uncalled 5];
             [Uncalled 1; Uncalled 12; Uncalled 20; Uncalled 15; Uncalled 19]];
        Finished
            42;
        Unfinished
            [[Uncalled 14; Uncalled 21; Uncalled 17; Uncalled 24; Uncalled 4];
             [Uncalled 10; Uncalled 16; Uncalled 15; Uncalled 9; Uncalled 19];
             [Uncalled 18; Uncalled 8; Uncalled 23; Uncalled 26; Uncalled 20];
             [Uncalled 22; Uncalled 11; Uncalled 13; Uncalled 6; Uncalled 5];
             [Uncalled 2; Uncalled 0; Uncalled 12; Uncalled 3; Uncalled 7]]]
}

let advanceBoard (callout : int) (board : Board) =
    match board with
    | Finished _ -> board
    | Unfinished rows ->
        rows
        |> List.map (
            List.map (
                fun cell ->
                    match cell with
                    | Called x -> Called x
                    | Uncalled x ->
                        if x = callout then
                            Called x
                        else
                            cell
            )
        )
        |> Unfinished

let checkBoard (callout : int) (board : Board) =
    let isCalled (cell : Cell) =
        match cell with
        | Called _ -> true
        | Uncalled _ -> false

    let checkRows (rows : Row list) =
        let hor =
            rows
            |> List.map (List.filter isCalled)
            |> List.contains []

        let ver =
            rows
            |> List.transpose
            |> List.map (List.filter isCalled)
            |> List.contains []

        hor || ver
    
    let calcScore (rows : Row list) =
        rows
        |> List.sumBy(
            List.sumBy(
                fun cell ->
                    match cell with
                    | Called   _ -> 0
                    | Uncalled x -> x
            )
        )
        |> fun x -> x * callout
        |> Finished

    match board with
    | Finished score -> Finished score
    | Unfinished rows ->
        match checkRows rows with
        | true -> calcScore rows
        | false -> Unfinished rows
        // rows
        // |> List.transpose

let advanceGame (game : GameState) =
    let advance (game : GameState) (callout : int) =
        game.Boards
        |> List.map (advanceBoard callout)
        |> List.map (checkBoard callout)

    match game.Callouts.Left with
    | callout::rest ->
        { game with
            Callouts = {
                Done = callout::game.Callouts.Done
                Left = rest
            }
            Boards = advance game callout
        }
    | _ -> failwith "whoops"

testGame
|> advanceGame
|> printGame