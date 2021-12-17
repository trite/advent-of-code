open System


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

let rawTestList =
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

let testList =
    rawTestList
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
    lst
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

type Next =
    | Done of int list
    | More of int list * int list list

let inner lst acc =
    let result =
        lst
        |> hasMore
        |> Option.map pop
        |> Option.map nextPart
    match result with
    | None -> Done acc
    | Some (gamma, rest) -> More (gamma::acc, rest)
    // writeLine result
    // result

let rec run' lst acc =
    match inner lst acc with
    | Done result      -> List.rev result
    | More (acc, rest) -> run' rest acc

let toChar (i : int) =
    i.ToString().[0]

let run lst = run' lst []

let gammaRaw = run testList

let toMeasurement raw =
    raw
    |> List.map toChar
    |> List.toArray
    |> (fun x -> String x)
    |> (fun x -> Convert.ToInt32(x, 2))

let gamma = 
    gammaRaw
    |> toMeasurement

let flip (lst : int list) =
    lst
    |> List.map (fun x ->
        match x with
        | 0 -> 1
        | 1 -> 0
        | _ -> failwith "Bad assumption in flip, this shouldn't be possible!"
    )

let epsilon =
    gammaRaw
    |> flip
    |> toMeasurement

$"Gamma  : {gamma}"
$"Epsilon: {epsilon}"
$"Result : {gamma * epsilon}"

let solve (lst : string list) =
    let gammaRaw =
        lst
        |> convert
        |> run

    let gamma = 
        gammaRaw
        |> toMeasurement 

    let epsilon =
        gammaRaw
        |> flip
        |> toMeasurement

    gamma * epsilon

solve rawTestList

let inputList =
    System.IO.File.ReadLines("2021\\Day03\\input.txt")
    |> Seq.toList

solve inputList
// answer: 3985686