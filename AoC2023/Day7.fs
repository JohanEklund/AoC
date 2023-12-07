module Day7

open AoCUtil

let test = [
    "32T3K 765"
    "T55J5 684"
    "KK677 28"
    "KTJJT 220"
    "QQQJA 483"
]

type Hand = { Cards: char list; Bid: int }

let parse (str: string) =
    let s = str.Split(" ")
    { Cards = s.[0] |> Seq.map(fun f -> f) |> Seq.toList; Bid = s.[1] |> int }

let isFiveOfAKind h =
    h.Cards.[0] = h.Cards.[1] && h.Cards.[1] = h.Cards.[2] && h.Cards.[2] = h.Cards.[3] && h.Cards.[3] = h.Cards.[4]

let isFourOfAKind h =
    let g = h.Cards |> List.groupBy(fun f -> f)
    let (_, g1) = g.[0]
    let (_, g2) = g.[1]
    g1.Length = 4 || g2.Length = 4

let isFullHouse h =
    let g = h.Cards |> List.groupBy(fun f -> f)
    let (_, g1) = g.[0]
    let (_, g2) = g.[1]
    (g1.Length = 3 && g2.Length = 2) || (g1.Length = 2 && g2.Length = 3)

let isThreeOfAKind h =
    let g = h.Cards |> List.groupBy(fun f -> f)
    let (_, g1) = g.[0]
    let (_, g2) = g.[1]
    if g1.Length = 3 || g2.Length = 3 then true
    else if g.Length < 3 then false
    else
        let (_, g3) = g.[2]
        g3.Length = 3

let isTwoPair h =
    h.Cards |> List.groupBy(fun f -> f)
        |> List.map(fun (_, f) -> f.Length) 
        |> List.where(fun f -> f = 2)
        |> fun f -> f.Length = 2

let isOnePair h =
    h.Cards |> List.groupBy(fun f -> f)
        |> List.map(fun (_, f) -> f.Length) 
        |> List.where(fun f -> f = 2)
        |> fun f -> f.Length = 1

let cardOrder c =
    match c with
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith $"unknown card {c}"

let cardOrder2 c =
    match c with
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'J' -> 1
    | 'Q' -> 11
    | 'K' -> 12
    | 'A' -> 13
    | _ -> failwith $"unknown card {c}"

let rec makeLists acc list =
    match list with
    | [] -> acc
    | hd::tl -> makeLists (
                    let (fives, fours, house, threes, twos, pair, rest) = acc
                    if isFiveOfAKind hd then (hd::fives, fours, house, threes, twos, pair, rest)
                    else if isFourOfAKind hd then (fives, hd::fours, house, threes, twos, pair, rest)
                    else if isFullHouse hd then (fives, fours, hd::house, threes, twos, pair, rest)
                    else if isThreeOfAKind hd then (fives, fours, house, hd::threes, twos, pair, rest)
                    else if isTwoPair hd then (fives, fours, house, threes, hd::twos, pair, rest)
                    else if isOnePair hd then (fives, fours, house, threes, twos, hd::pair, rest)
                    else (fives, fours, house, threes, twos, pair, hd::rest)
                ) tl

let rec makeLists2 acc list =
    match list with
    | [] -> acc
    | hd::tl -> makeLists2 (
                    let jokers = hd.Cards |> List.where(fun f -> f = 'J') |> List.length
                    let (fives, fours, house, threes, twos, pair, rest) = acc
                    if isFiveOfAKind hd || (jokers = 1 && isFourOfAKind hd) || ( jokers = 2 && isThreeOfAKind hd) || (jokers = 3 && isOnePair hd || jokers = 4) then (hd::fives, fours, house, threes, twos, pair, rest)
                    else if isFourOfAKind hd || (jokers = 1 && isThreeOfAKind hd) || (jokers = 2 && isTwoPair hd) || jokers = 3 then (fives, hd::fours, house, threes, twos, pair, rest)
                    else if isFullHouse hd || (jokers = 1 && isTwoPair hd) then (fives, fours, hd::house, threes, twos, pair, rest)
                    else if isThreeOfAKind hd || (jokers = 1 && isOnePair hd) || jokers = 2 then (fives, fours, house, hd::threes, twos, pair, rest)
                    else if isTwoPair hd then (fives, fours, house, threes, hd::twos, pair, rest)
                    else if isOnePair hd || jokers = 1 then (fives, fours, house, threes, twos, hd::pair, rest)
                    else (fives, fours, house, threes, twos, pair, hd::rest)
                ) tl

let sortList l =
    l |> List.sortBy(fun f -> f.Cards |> List.map cardOrder |> (fun f -> f.[0] * 100000000 + f.[1] * 1000000 + f.[2] * 10000 + f.[3] * 100 + f.[4] * 1))

let sortList2 l =
    l |> List.sortBy(fun f -> f.Cards |> List.map cardOrder2 |> (fun f -> f.[0] * 100000000 + f.[1] * 1000000 + f.[2] * 10000 + f.[3] * 100 + f.[4] * 1))

let day7A = 
    let hands =     
            // test
            readLines @".\input.txt"
                |> List.map parse
    let (a, b, c, d, e, f, g) = makeLists ([], [], [], [], [], [], []) hands
    let g1 = g |> sortList |> List.mapi(fun i x -> x.Bid * (i+1)) |> List.sum
    let f1 = f |> sortList |> List.mapi(fun i x -> x.Bid * (i+g.Length+1)) |> List.sum
    let e1 = e |> sortList |> List.mapi(fun i x -> x.Bid * (i+g.Length+f.Length+1)) |> List.sum
    let d1 = d |> sortList |> List.mapi(fun i x -> x.Bid * (i+g.Length+f.Length+e.Length+1)) |> List.sum
    let c1 = c |> sortList |> List.mapi(fun i x -> x.Bid * (i+g.Length+f.Length+e.Length+d.Length+1)) |> List.sum
    let b1 = b |> sortList |> List.mapi(fun i x -> x.Bid * (i+g.Length+f.Length+e.Length+d.Length+c.Length+1)) |> List.sum
    let a1 = a |> sortList |> List.mapi(fun i x -> x.Bid * (i+g.Length+f.Length+e.Length+d.Length+c.Length+b.Length+1)) |> List.sum
    (g1 + f1 + e1 + d1 + c1 + b1 + a1)
    
let day7B = 
    let hands =     
            // test
            readLines @".\input.txt"
                |> List.map parse
    let (a, b, c, d, e, f, g) = makeLists2 ([], [], [], [], [], [], []) hands
    let g1 = g |> sortList2 |> List.mapi(fun i x -> x.Bid * (i+1)) |> List.sum
    let f1 = f |> sortList2 |> List.mapi(fun i x -> x.Bid * (i+g.Length+1)) |> List.sum
    let e1 = e |> sortList2 |> List.mapi(fun i x -> x.Bid * (i+g.Length+f.Length+1)) |> List.sum
    let d1 = d |> sortList2 |> List.mapi(fun i x -> x.Bid * (i+g.Length+f.Length+e.Length+1)) |> List.sum
    let c1 = c |> sortList2 |> List.mapi(fun i x -> x.Bid * (i+g.Length+f.Length+e.Length+d.Length+1)) |> List.sum
    let b1 = b |> sortList2 |> List.mapi(fun i x -> x.Bid * (i+g.Length+f.Length+e.Length+d.Length+c.Length+1)) |> List.sum
    let a1 = a |> sortList2 |> List.mapi(fun i x -> x.Bid * (i+g.Length+f.Length+e.Length+d.Length+c.Length+b.Length+1)) |> List.sum
    (g1 + f1 + e1 + d1 + c1 + b1 + a1)
    