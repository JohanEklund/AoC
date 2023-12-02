module Day2

open AoCUtil

type CubeReveal = { Red : int; Green : int; Blue : int }
type GameData = { Id : int; Reveals : list<CubeReveal> }
type MinimumCubes = { Id : int; Min : CubeReveal }

let test = [
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
]

let getMinimum data =
    let maxBlue = data.Reveals |> List.map (fun f -> f.Blue) |> List.max
    let maxRed = data.Reveals |> List.map (fun f -> f.Red) |> List.max
    let maxGreen = data.Reveals |> List.map (fun f -> f.Green) |> List.max
    { Id = data.Id; Min = { Red = maxRed; Blue = maxBlue; Green = maxGreen}}

let parseCubeColor (str: array<string>) (color: string) =
    str
        |> Seq.where (fun s -> s.Contains(color))
        |> fun s -> match s |> Seq.toList with
                    | hd::_ -> hd.Replace(color, "") |> int
                    | [] -> 0
        

let parseCubeReveal (str: string) =
    let cubes = str.Split(",")
    let blue = parseCubeColor cubes "blue"
    let red = parseCubeColor cubes "red"
    let green = parseCubeColor cubes "green"
    { Red = red; Green = green; Blue = blue }

let rec parseCubeRevealsRec str acc =    
    match str with
    | hd::tl -> parseCubeRevealsRec tl ((parseCubeReveal hd) :: acc)
    | [] -> acc

let parseCubeRevealsStart (str: string) =    
    parseCubeRevealsRec (str.Split(";") |> Array.toList) []

let parseGameData (str : string) =
    let split = str.Split(":")
    let id = split.[0].Replace("Game ", "") |> int    
    let reveals = parseCubeRevealsStart split.[1]    
    { Id = id; Reveals = reveals }

let filterGameData filter input =
    let any = input.Reveals |> List.map (fun f -> f.Red <= filter.Red && f.Blue <= filter.Blue && f.Green <= filter.Green) |> List.fold (fun acc f -> acc && f) true
    if any
    then Some input
    else None
        

let day2A =
    // test
    readLines @".\input.txt"
        |> Seq.map parseGameData
        |> Seq.map(fun f -> filterGameData { Red = 12; Green = 13; Blue = 14 } f)
        |> Seq.choose id
        |> Seq.map (fun f -> f.Id)
        |> Seq.sum

let day2B =
    // test
    readLines @".\input.txt"
        |> Seq.map parseGameData
        |> Seq.map getMinimum
        |> Seq.map (fun f -> f.Min.Blue * f.Min.Green * f.Min.Red)
        |> Seq.sum

