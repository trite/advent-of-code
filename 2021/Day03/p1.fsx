let writeLine (o : obj) =
    System.Console.WriteLine(o)

let simpleRaise str =
    raise (System.Exception(str))

let toCharArray (str : string) =
    str.ToCharArray()

let charToInt (c : char) =
    c.ToString() |> int

let convert lst =
    lst
    |> List.map (
        toCharArray
        >> Array.toList
        >> List.map charToInt
    )

let testList =
    [
        "00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"
    ]
    |> convert

let hasMore lst =
    lst
    |> List.head
    |> List.tryHead
    |> Option.map (fun _ -> lst)

let tuplify lst =
    match lst with
    | x::rest -> (x,rest)
    | _ -> simpleRaise "Bad assumption about tuplify"

let compactify lst =
    (List.map fst lst, List.map snd lst)

let pop lst =
    testList
    |> List.map tuplify
    |> compactify

pop testList

let intList = [0; 1; 1; 1; 1; 0; 0; 1; 1; 1; 0; 0]

let getGamma lst =
    lst
    |> List.countBy id
    |> List.maxBy (fun (_, x) -> x)
    |> fst

let nextPart (lst : int list, rest : int list list) =
    (getGamma lst, rest)


let inner lst acc =
    let result =
        lst
        |> hasMore
        |> Option.map pop
        |> Option.map nextPart
    writeLine result
    result
    // match result with
    // | None -> List.rev acc
    // | Some (gamma, rest) -> inner rest (gamma::acc)

let run x =
    inner x []

// run testList

let x = inner testList []
let y = Option.map (fun (g, rest) -> inner rest g) x 
x
testList

// Option.map (fun (g, rest) -> rest) x