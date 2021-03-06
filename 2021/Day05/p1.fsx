let split (c : string) (str : string) =
    str.Split c
    |> Array.toList

let testInput =
    """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

type CoordPair =
    {
        x1 : int
        y1 : int
        x2 : int
        y2 : int
    }

let testList =
    testInput
    |> split "\n"

let parse (lst : string list) =
    let tuplify (lst : 'a list) =
        match lst with
        | [x;y] -> (x,y)
        | _     -> failwith "This shouldn't happen... check your logic!"

    let toCoordPair ((x1,y1),(x2,y2)) =
        {
            x1 = x1 |> int
            y1 = y1 |> int
            x2 = x2 |> int
            y2 = y2 |> int
        }
        
    lst
    |> List.map (
        split " -> "
        >> List.map (split "," >> tuplify)
        >> tuplify
        >> toCoordPair
    )

parse testList

let newList length =
    Array2D.zeroCreate length length

// Using active patterns as a form of self-documentation:
let applyCoords (coords : CoordPair) arr =
    let (|Horizontal|Vertical|DoNothing|) (coords : CoordPair) =
        match (coords.x1, coords.x2, coords.y1, coords.y2) with
        | (_,_,y1,y2) when y1 = y2 -> Horizontal
        | (x1,x2,_,_) when x1 = x2 -> Vertical
        | _                        -> DoNothing

    match coords with
    | Horizontal -> failwith "Logic for horizontal lines goes here"
    | Vertical   -> failwith "Logic for vertial lines in this branch"
    | DoNothing  -> arr