let split ( c : char ) ( str : string ) =
    str.Split(c)
    |> Array.toList

let trim ( str : string ) =
    str.Trim()

type Position =
    {
        horizontal : int
        depth : int
        aim : int
    }

let testList =
    "forward 5
    down 5
    forward 8
    up 3
    down 8
    forward 2"
    |> split '\n'
    |> List.map trim // un-indenting the string literal is too ugly to permit
    |> List.map (split ' ')

let run (dir : string) (x : int) (pos : Position) =
    match (dir, x) with
    | ("forward", x) ->
        {
            pos with
                horizontal = pos.horizontal + x
                depth = pos.depth + (pos.aim * x)
        }
    | ("down", x) ->
        { pos with aim = pos.aim + x }
    | ("up", x) ->
        { pos with aim = pos.aim - x }
    | _ -> raise (System.Exception("Missed something with run"))

let runList (lst : string list list) =
    let rec recurse (pos : Position) (items : string list list) =
        match items with
        | [] -> pos
        | ([way;dist]::rest) -> recurse (run way (int dist) pos) rest
        | _ -> raise (System.Exception("Missed something with runList"))
    
    recurse { horizontal = 0; depth = 0; aim = 0 } lst

let calcResult (pos : Position) =
    pos.horizontal * pos.depth

let testPosition = runList testList
let testResult = testPosition.depth * testPosition.horizontal

// FSI runs from VSCode project folder root by default
System.IO.File.ReadLines("2021\\Day02\\input.txt")
|> Seq.toList
|> List.map (split ' ')
|> runList
|> calcResult
// answer: 1544000595