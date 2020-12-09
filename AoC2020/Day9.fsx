#load "./utils.fsx"
open AoCUtil

let testInput = [
        "35";
        "20";
        "15";
        "25";
        "47";
        "40";
        "62";
        "55";
        "65";
        "95";
        "102";
        "117";
        "150";
        "182";
        "127";
        "219";
        "299";
        "277";
        "309";
        "576" 
    ]

let rec find x (orderedList: int64 list) =
    if (orderedList.[0] + orderedList.[0]) > x  then false
    else
        let firstTerm = x - orderedList.[0]
        let secondTermExists = orderedList |> List.exists (fun e -> ((x - e) = firstTerm))
        if secondTermExists then true
        else
            match orderedList with
            | [] -> false
            | (_::tl) -> find x tl

let isSumOfPrev x prev25 =
    let ordered = prev25 |> List.sort
    let res = find x ordered
    (res, x)

let addToPrev x prev =
    match prev with
    | (_::tl) -> tl @[x]
    | [] ->
            printfn "BAD"
            [x]

let rec findFirstNotSum prev input =
    match input with
    | [] -> int64 -1
    | (hd::tl) ->
        let (x, y) = isSumOfPrev hd prev
        if not x then y
        else
            let newPrev = addToPrev hd prev
            findFirstNotSum newPrev tl

let solveA input preambleSize =
    let all = input |> List.map(int64)
    let initial =  all |> List.take preambleSize
    let input = all |> List.skip preambleSize |> List.take (all.Length - preambleSize)
    findFirstNotSum initial input

let example1 =
    solveA testInput 5


let a =
    solveA (readLines "./AoC2020/input9.txt") 25