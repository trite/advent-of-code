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

let testList =
    testInput.Split '\n'
    |> List.ofArray

// Not sure the best approach here, borrowing from how I ended up splitting strings in haskell (2015 day 2 part 1)
/// Split a string list into sub-lists, splitting on the specified string
let splitOn split lst =
    let rec splitOn' accList acc split (lst : string list) =
        match lst with
        | []    -> (List.rev acc)::accList |> List.rev
        | x::xs ->
            if split = x then
                splitOn' (List.rev acc::accList) [] split xs
            else
                splitOn' accList (x::acc) split xs
    splitOn' [] [] split lst

splitOn "" testList
|> List.splitAt 1
|> fun (x,y) ->
    let callouts = (x |> List.head |> List.head).Split ','
    callouts

splitOn "" testList
|> List.splitAt 1
|> fun (x,y) ->
    y
    |> List.map (
        fun il ->
            List.map (
                fun lst ->
                    // this feels like a terrible way to go...
                    // TODO: pick up here, try to make this suck less if possible too
            )

    )


//     let split = splitOn "" lst

        // |> List.splitAt 1
        // |> fun (x,y) -> (List.head x, y)