let fsiAssert (assertion : bool) (message : string) : unit =
    if not assertion then
        failwith message

let testMap =
    Map [
        ("(())", 0)
        ("()()", 0)
        ("(((", 3)
        ("(()(()(", 3)
        ("))(((((", 3)
        ("())", -1)
        ("))(", -1)
        (")))", -3)
        (")())())", -3)
    ]

let countLine (line : string) =
    let mapChar (c : char) : int =
        match c with
        | '(' -> 1
        | ')' -> -1
        | _   -> failwith $"Expected '(' or ')' but received {c}"
    
    line.ToCharArray()
    |> Array.toList
    |> List.map mapChar
    |> List.sum

testMap
|> Map.iter (
    fun k v ->
        let result = countLine k
        printfn $"expected: {v}, received: {result}"
        fsiAssert (result = v) $"Expected {v} but received {result}!"
)

System.IO.File.ReadAllText("2015\\Day01\\input.txt")
|> countLine
|> printfn "Answer: %A"
// Answer: 232