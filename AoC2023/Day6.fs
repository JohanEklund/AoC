module Day6

open AoCUtil

let test = [
    "Time:      7  15   30"
    "Distance:  9  40  200"
]

let parse (str: string) =
    str.Split(" ")
        |> Seq.skip 1
        |> Seq.where(fun f -> f <> "")
        |> Seq.map(fun x -> x |> int64)

let parse2 (str: string) =
    str.Split(" ")
        |> Seq.skip 1
        |> Seq.where(fun f -> f <> "")
        |> Seq.fold(fun a b -> $"{a}{b}") ""
        |> int64

let distance speed time =
    speed * time

let calc race =
    let rec getPossibilities t d s p=
        if distance s (t-s) > d 
        then getPossibilities t d (s+1L) (p+1L)
        else if p = 0L then getPossibilities t d (s+1L) p
        else p
    let (time, distance) = race
    getPossibilities time distance 0 0

let day6A =
    let input = 
        // test
        readLines @".\input.txt" 
            |> List.map parse
    Seq.zip input.[0] input.[1]
    |> Seq.map calc
    |> Seq.fold(fun a b -> a * b) 1L

let day6B =
    // let a = test
    let a = readLines @".\input.txt" 
            |> Seq.map parse2
            |> Seq.toList
    calc (a.[0], a.[1])
        