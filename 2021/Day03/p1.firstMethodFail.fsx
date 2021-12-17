let charToInt o =
    o.ToString() |> int

let toCharArray (str : string) =
    str.ToCharArray()

let toString o =
    o.ToString()

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
    |> List.map toCharArray

let checkMore (_ : , lst) =
    lst
    |> List.head
    |> toCharArray
    |> Array.tryHead
    |> Option.map (fun _ -> lst)

let tuplify arr =
    let lst = Array.toList arr
    match lst with
    | x::rest -> (x,rest)
    | _ -> raise (System.Exception(""))

// let onFst (f : char -> 'a) (x : char, y : char list) : 'a * char list =
let onFst f (x,y) =
    (f x, y)

// let compactify (x : (int * char list) list option) =
//     let first (lst : (int * char list) list) = List.map fst lst
//     let second (lst : (int * char list) list) = List.map snd lst
//     Option.map (fun t -> (first t, second t)) x
    // match x with
    // | Some thing -> Some (first thing, second thing)
    // | None -> None
    // (first x, second x)


let compactify (x : (int * char list) list) =
    (List.map fst x, List.map snd x)



let pop lst =
    let work inList =
        inList
        |> List.map (fun x ->
                x
                // |> toCharArray
                |> tuplify
                |> onFst charToInt
            )
    Option.map work (checkMore lst)
    |> Option.map compactify

pop testList