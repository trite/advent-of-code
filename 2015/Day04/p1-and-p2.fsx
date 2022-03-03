open System.Security.Cryptography
open System.Text

let md5 (toHash : string) : string =
    use md5Hash = MD5.Create()
    toHash
    |> Encoding.ASCII.GetBytes
    |> md5Hash.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let testAnswer (secretKey : string) (number : int) (testFor : string) : bool =
    let hashed = md5 $"{secretKey}{number}"
    hashed.StartsWith(testFor)

let p1Zeroes = String.replicate 5 "0"

testAnswer "abcdef" 609043 p1Zeroes
testAnswer "pqrstuv" 1048970 p1Zeroes

let findAnswer (testFor : string) (secretKey : string) : int =
    let rec findAnswer' (secretKey : string) (number : int) (testFor : string) : int =
        match testAnswer secretKey number testFor with
        | true -> number
        | false -> findAnswer' secretKey (number + 1) testFor

    findAnswer' secretKey 0 testFor
        
let findAnswerP1 = findAnswer p1Zeroes

findAnswerP1 "ckczppom"
|> printfn "Part 1 answer: %A"
// Part 1 answer: 117946

let findAnswerP2 = findAnswer (String.replicate 6 "0")

findAnswerP2 "ckczppom"
|> printfn "Part 2 answer: %A"
// Part 2 answer: 3938038