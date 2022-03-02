let testList =
    Map [
        ("2x3x4", 58)
        ("1x1x10", 43)
    ]

let split ( c : char ) ( str : string ) =
    str.Split(c)
    |> Array.toList

let convert (dimensionsStr : string) : int list =
    split 'x' dimensionsStr
    |> List.map int
    |> List.sort


let doCalc (dimensionsStr : string) (calculation : int * int * int -> int) : int =
    let dimensions = convert dimensionsStr

    match dimensions with
    | [a;b;c] -> calculation (a,b,c)
    | _       -> failwith $"This should only ever be 3 elements, but instead was {dimensions.Length}! Check your logic."

let calcSqFt (dimensionsStr : string) : int =
    let calculation =
        fun (a,b,c) -> 3*a*b + 2*a*c + 2*b*c

    doCalc dimensionsStr calculation

let testPt1 (dimensionsStr : string) (expectedResult : int) : unit =
    let result = calcSqFt dimensionsStr
    if result <> expectedResult then
        failwith $"Expected {expectedResult} but received {result}"

// Sanity check
testList
|> Map.iter testPt1

let runCalculation (calc : string -> int) : int=
    System.IO.File.ReadLines("2015\\Day02\\input.txt")
    |> Seq.toList
    |> List.map calc
    |> List.sum

runCalculation calcSqFt
|> printfn "Part 1 answer: %A"
// Part 1 answer: 1606483

let calcRibbon (dimensionsStr : string) : int =
    let calculation =
        fun (a,b,c) -> 2*a + 2*b + a*b*c

    doCalc dimensionsStr calculation

runCalculation calcRibbon
|> printfn "Part 2 answer: %A"
// Part 2 answer: 3842356