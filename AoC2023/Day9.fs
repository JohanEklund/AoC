module Day9

open AoCUtil

let test = [
    "0 3 6 9 12 15"
    "1 3 6 10 15 21"
    "10 13 16 21 30 45"
]

let parse (str: string list) =
    str |> List.map (fun f -> f.Split(" ") |> Seq.map int)
    
// let input = test
let input = readLines @".\input.txt"

let generateDiffs list =
    let rec generateDiffsRec acc list =
        match list with
        | [] -> acc
        | a::b::tl -> (b-a)::generateDiffsRec acc (b::tl)
        | x -> acc
    generateDiffsRec [] list

let allTheDiffs start list =
    let isDone list = list |> List.fold(fun a v -> a && v = 0) true
    let rec loop acc list =
        if isDone list then acc
        else
            let newDiff = generateDiffs list
            loop (newDiff::acc) newDiff
    loop start list

let day9A = 
    input
        |> parse
        |> Seq.map(fun x -> x |> Seq.toList)
        |> Seq.map(fun x -> (allTheDiffs [x] x))
        |> Seq.map(fun l -> l |> Seq.fold(fun a v -> a + (v |> Seq.rev |> Seq.toList).Head) 0)
        |> Seq.sum
        
let day9B = 
    input
        |> parse
        |> Seq.map(fun x -> x |> Seq.toList)
        |> Seq.map(fun x -> (allTheDiffs [x] x))
        |> Seq.map(fun l -> l |> Seq.fold(fun a v -> ((v |> Seq.toList).Head) - a) 0)
        |> Seq.sum