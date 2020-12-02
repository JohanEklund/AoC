#load "./utils.fsx"

open AoCUtil

type Password = { Min: int; Max: int; Req: char; Password: string }

let debug = ["1-3 a: abcde"; "1-3 b: cdefg"; "2-9 c: ccccccccc"];

let input = readLines "./AoC2020/input2.txt"

let parse (s: string) =
    let first = s.Split("-");
    let min = first |> Array.head |> int
    let second = first.[1].Split(" ")
    let max = second |> Array.head |> int
    let req = second.[1].Replace(":", "").[0]
    let pass = second.[2]
    {Min = min; Max = max; Req = req; Password = pass}

let matchingCharacter x y =
    if x.Equals(y)
    then 1
    else 0

let isValid1 p =
    let count = Seq.toList p.Password
                |> List.sumBy(fun e -> matchingCharacter e p.Req)
    if count >= p.Min && count <= p.Max
    then 1
    else 0
    
let isValid2 p =
    let first = p.Password.[p.Min - 1].Equals(p.Req)
    let second = p.Password.[p.Max - 1].Equals(p.Req)
    if first && second then 0
    else if first || second then 1
    else 0 


let a = List.sumBy isValid1 (input
        |> List.map (parse))

        
let b = List.sumBy isValid2 (input
        |> List.map (parse))