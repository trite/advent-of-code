let p1Tests (input : string) : bool =
    let testVowels (input : string) : bool =
        let vowelFilter (c : char) : bool =
            List.exists (fun x -> x = c) ['a';'e';'i';'o';'u']

        let result = 
            input.ToCharArray()
            |> Array.filter vowelFilter
            |> Array.length

        result >= 3

    let testDoubleLetter (input : string) : bool =
        let verify (c : char array) =
            match c with
            | [| x; y |] -> x = y
            | bad        -> failwith $"Expected a 2 element array but received: {bad}"

        let result = 
            input.ToCharArray()
            |> Array.windowed 2
            |> Array.filter verify

        result.Length > 0

    let testDealBreaker (input : string) : bool =
        let badPairs = [('a','b'); ('c','d'); ('p', 'q'); ('x', 'y')]
        let verify (c : char array) : bool =
            match c with
            | [| x; y |] -> List.contains (x,y) badPairs
            | bad        -> failwith $"Expected a 2 element array but received: {bad}"
        
        let result = 
            input.ToCharArray()
            |> Array.windowed 2
            |> Array.filter verify
        
        result.Length = 0

    testVowels input
    && testDoubleLetter input
    && testDealBreaker input

// List.map p1Tests [
//     "ugknbfddgicrmopn"
//     "aaa"
//     "jchzalrnumimnmhp"
//     "haegwjzuvuyypxyu"
//     "dvszwmarrgswjxmb"
// ]

let run (func : string -> bool) : int =
    System.IO.File.ReadLines("2015\\Day05\\input.txt")
    |> Seq.map func
    |> Seq.sumBy (fun x -> if x = true then 1 else 0)

run p1Tests
|> printfn "Part 1 answer: %A"