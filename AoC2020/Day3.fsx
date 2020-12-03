#load "./utils.fsx"

open AoCUtil

let debug = [
    "..##.......";
    "#...#...#..";
    ".#....#..#.";
    "..#.#...#.#";
    ".#...##..#.";
    "..#.##.....";
    ".#.#.#....#";
    ".#........#";
    "#.##...#...";
    "#...##....#";
    ".#..#...#.#";
]

let input = readLines "./AoC2020/input3.txt"

let isTree (x: char) =
    if x.Equals('#')
    then 1
    else 0

let posOnRow row width slope =
    (row * slope) % width

let isTreeAtPos pos (str: string) =
    isTree str.[pos]

let countTrees slope (grid: string list) =
    grid
        |> List.mapi (fun i e -> isTreeAtPos (posOnRow i e.Length slope) e)
        |> List.sum

let a = input 
        |> countTrees 3


let b : int64 = 
    let x1 = input
            |> countTrees 1
    let x2 = input
            |> countTrees 3
    let x3 = input
            |> countTrees 5
    let x4 = input
            |> countTrees 7
    let x5 = input
            |> List.mapi (fun i e ->
                match i % 2 with
                    | 0 -> Some e
                    | _ -> None)
            |> List.choose (id)
            |> countTrees 1
    int64(x1) * int64(x2) * int64(x3) * int64(x4) * int64(x5)