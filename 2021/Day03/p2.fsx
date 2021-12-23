open System

let toCharArray (str : string) =
    str.ToCharArray()

let charToInt (c : char) =
    c.ToString() |> int

let str2intList (str : string) =
    str
    |> toCharArray
    |> Array.toList
    |> List.map charToInt

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
    |> List.map str2intList

let rec mainWork lst filter pos =
    match List.length lst with
    | 1 -> List.head lst
    | 0 -> failwith "This wasn't supposed to happen, you made a bad assumption!"
    | _ -> 
        let toKeep =
            lst
            |> List.transpose
            |> List.item pos
            |> List.countBy id
            |> filter
        let keep =
            lst
            |> List.filter (fun x -> List.item pos x = toKeep)
        mainWork keep filter (pos + 1)

// active pattern for this feels silly, just wanted to learn
let (|Same|Diff|) (m : Map<int,int>) =
    if m.[0] = m.[1] then
        Same
    else
        Diff

let o2 (lst : (int * int) list) =
    match Map.ofList lst with
    | Same -> 1
    | Diff ->
        lst
        |> List.maxBy snd
        |> fst

let co2 (lst : (int * int) list) =
    match Map.ofList lst with
    | Same -> 0
    | Diff ->
        lst
        |> List.minBy snd
        |> fst

let toChar (i : int) =
    i.ToString().[0]

let toMeasurement raw =
    raw
    |> List.map toChar
    |> List.toArray
    |> (fun x -> String x)
    |> (fun x -> Convert.ToInt32(x, 2))

let run lst =
    let o2Rating = mainWork lst o2 0 |> toMeasurement
    let co2Rating = mainWork lst co2 0 |> toMeasurement
    o2Rating * co2Rating

run testList

let inputList =
    System.IO.File.ReadLines("2021\\Day03\\input.txt")
    |> Seq.toList
    |> List.map str2intList

run inputList
// answer: 2555739