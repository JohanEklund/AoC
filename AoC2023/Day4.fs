module Day4

open AoCUtil

type Card = { Title: string; WinningNumbers : int Set; ActualNumbers : int Set }

let test = [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
]

let parseCard (str: string) =
    let ss = str.Split(":")
    let s = ss.[1].Split("|")
    let w = s.[0].Split(" ") |> Seq.where(fun s -> s <> "") |> Seq.map(fun f -> f |> int) |> Set
    let a = s.[1].Split(" ") |> Seq.where(fun s -> s <> "") |> Seq.map(fun f -> f |> int) |> Set
    { Title = ss.[0]; WinningNumbers = w; ActualNumbers = a }

let calculatePoints card =
    let w = Set.intersect card.WinningNumbers card.ActualNumbers
    match w.Count with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 2
    | x -> pown 2 (x-1)

let numberOfWins card =
    let r = Set.intersect card.WinningNumbers card.ActualNumbers
    r.Count

let copyCards l =
    l
        |> Seq.map numberOfWins
        |> Seq.map(fun f -> f, 1)
        
let addDupes (l: (int*int) list) =
    let rec addDupesRec list wins numToAdd =        
        if wins = 0 then list
        else
            match list with
            | [] -> []
            | (x,y)::tl -> (x, y+numToAdd)::(addDupesRec tl (wins-1) numToAdd)
    let rec loop list =
        match list with
        | [] -> list
        | (a,b)::tl -> (a,b)::loop (addDupesRec tl a b)
    loop l

let day4A =
    // test
    readLines @".\input.txt" 
        |> Seq.map parseCard
        |> Seq.map calculatePoints
        |> Seq.sum

let day4B =
    // test
    readLines @".\input.txt" 
        |> Seq.map parseCard
        |> copyCards
        |> Seq.toList
        |> addDupes
        |> Seq.map(fun (_,b) -> b ) 
        |> Seq.sum
