let checkChar (c : char) : int =
    match c with
    | '(' -> 1
    | ')' -> -1
    | _   -> failwith $"Expected '(' or ')' but received {c}!"

let rec findBasement' (count : int) (floor : int) (lst : char list) : int =
    if floor = -1 then
        count
    else
        match lst with
        | [] -> failwith $"Reached the end of list without hitting basement... Check your logic!"
        | x::rest -> findBasement' (count + 1) (floor + checkChar x) rest

let findBasement (lst : char list) : int =
    findBasement' 0 0 lst

System.IO.File.ReadAllText("2015\\Day01\\input.txt").ToCharArray()
|> Array.toList
|> findBasement
|> printfn "Answer: %A"
// Answer: 1783