type Callouts = Callouts of int list
type Row = Row of (int * bool) list
type Board = Board of Row list
type Boards = Boards of Board list

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

let split (c : char) (str : string) =
    str.Split c
    |> Array.toList

let testList =
    testInput
    |> split '\n'

// Not sure the best approach here, borrowing from how I ended up splitting strings in haskell (2015 day 2 part 1)
/// Split a string list into sub-lists, splitting on the specified string
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

/// Parses input info into a callout list and a board list
let parse (lst : string list) =
    splitOn "" lst
    |> List.splitAt 1
    |> fun (cRaw,bRaw) ->
        let callouts =
            cRaw
            |> List.head
            |> List.head
            |> split ','
            |> List.map int
            |> Callouts

        let boards =
            bRaw
            |> List.map (
                List.map (
                    split ' '
                    >> List.filter (fun x -> x <> "")
                    >> List.map (fun x -> (int x, false))
                    >> Row
                )
                >> Board
            )
            |> Boards

        (callouts, boards)

let advanceBoards (Boards boards : Boards) (callout : int) =
    boards
    |> List.map (
        fun (Board board : Board) ->
            board
            |> List.map (
                fun (Row row : Row) ->
                    row
                    |> Map.ofList
                    |> Map.change callout (Option.map (fun _ -> true))
                    |> Map.toList
                    |> Row
            )
            |> Board
    )
    |> Boards

let checkBoards (Boards boards : Boards) =
    let checkBoard (Board board : Board) =
        let isEmpty lst =
            (List.length lst) = 0

        let called (_, b : bool) =
            b

        let winners (x : (int * bool) list) =
            x
            |> List.filter called
            |> isEmpty

        // this is only needed to deal with the type setup I created
        // is there a better way to handle the types to skip work like this?
        // might also be able to skip lots of the "boxing" steps (|> Boards)
        let extract (Row row : Row) =
            row

        let checkRows =
            board
            |> List.map extract
            |> List.filter winners
            |> isEmpty

        let checkCols =
            board
            |> List.map extract
            |> List.transpose
            |> List.filter winners
            |> isEmpty
        
        checkRows || checkCols
        
    boards
    |> List.filter checkBoard
    |> fun boards ->
        if (List.length boards) = 0 then
            None
        else
            Some (boards |> List.head)

let (Callouts callouts, boards) = parse testList

// callouts
// |> List.head
// |> advanceBoards boards
// |> checkBoards

// Wonder which approach is more idiomatic.
// Taking tupled input avoids ||> or other extra steps, so going with it for now.
// let runGame (Callouts callouts : Callouts) (Boards boards : Boards) =
let runGame ((Callouts callouts : Callouts),(Boards boards : Boards)) =
    let calcScore (Board board : Board) (lastCallout : int) =
        let boardSum =
            board
            |> List.sumBy (
                fun (Row row : Row) ->
                    row
                    |> List.sumBy (
                        fun (num : int, b : bool) ->
                            if b then
                                num
                            else
                                0
                    )
            )
        boardSum * lastCallout

    let sanityCheck (lst : 'a list) =
        match lst with
        | [] -> failwith "callout list is empty before it should be, check your logic!"
        | _  -> lst

    let rec runGame' ((callout : int) :: (rest : int list)) (boards : Boards) =
        let newBoards =
            advanceBoards boards callout
        
        match checkBoards newBoards with
        | None -> runGame' (sanityCheck rest) newBoards
        | Some winningBoard -> calcScore winningBoard callout

    boards
    |> sanityCheck
    |> Boards // I believe needing this is another symptom of my choice of types, can I make it suck less?
    |> runGame' callouts

testList
|> parse
// ||> runGame
|> runGame

// TODO:
// * Add logic to check if a board has won
// * Run till a board wins (advance -> check -> repeat)
// * Calculate the answer for the winning board
// * ???
// * Profit