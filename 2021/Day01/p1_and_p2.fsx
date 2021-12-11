let run (lst : int list) =
    lst
    |> List.pairwise
    |> List.filter (fun (x,y) -> y > x)
    |> List.length

let actualList =
    System.IO.File.ReadLines("2021\\Day01\\input.txt")

actualList
|> Seq.toList
|> List.map int
|> run
// p1 answer: 1676

actualList
|> Seq.toList
|> List.map int
|> List.windowed 3
|> List.map List.sum
|> run
// p2 answer: 1706

(* Alternately to solve the first part this is effectively all that's happening:
System.IO.File.ReadLines("2021\\Day01\\input.txt")
|> Seq.toList
|> List.map int
|> List.pairwise
|> List.filter (fun (x,y) -> y > x)
|> List.length
*)