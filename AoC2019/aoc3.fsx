#load "./AoCUtil.fsx"

open AoCUtil

type Coordinat = { X : int; Y : int; }

type Command = { Direction: char; Number: int;}

let MoveUp start = 
    { X= start.X; Y = (start.Y + 1)}
   
let MoveDown start =
    { X= start.X; Y = (start.Y - 1)}

let MoveLeft start =
    { X= (start.X - 1); Y = start.Y}

let MoveRight start =
    { X= (start.X + 1); Y = start.Y}

let Invalid x: Coordinat = 
    { X = -1; Y = -1 }

let rec Move dir (start: Coordinat) number moves =
    let newPos = dir start    
    match number with
    | 0 -> moves
    | _ -> Move dir newPos (number - 1) (newPos::moves)

let getDirection command =
    command.Direction
    |> function
        | 'U' -> MoveUp
        | 'D' -> MoveDown
        | 'L' -> MoveLeft
        | 'R' -> MoveRight
        | _ -> Invalid

let getNumber (command : string) = 
    command.[1..]
    |> int
    
let getStr (command: string) =
    command.[0]

let parse str =
    { Direction = getStr str; Number = getNumber str }

let parseList listStr =
    listStr
    |> List.map parse


let doCommand command moves =
    Move (getDirection command) (List.head moves) command.Number moves

let rec doCommands commands moves = 
    match commands with
    | [] -> moves
    | _ -> doCommands commands.[1..] (doCommand commands.[0] moves)

let emptyList : Coordinat list =
    [{X = 0; Y = 0}]

let getCommands fileName = 
    readCommaSeparatedStr fileName
    |> List.map parseList

let rec existsInList list coord =
    match list with
    | [] -> false
    | head::tail -> 
        if coord = head then true
        else existsInList tail coord

let filterLists list1 list2 =
    list1
    |> List.filter (fun e -> existsInList list2 e)

let zeroCoord =
    { X = 0; Y = 0}

let calcDist coord = 
    if coord = zeroCoord then 999
    else System.Math.Abs coord.X + System.Math.Abs coord.Y

// let UpgA =
//     let inputFile = "./AoC2019/Day3/input.txt"
//     let commands = getCommands inputFile
//     let l1 = doCommands commands.[0] emptyList
//     let l2 = doCommands commands.[1] emptyList
//     filterLists l1 l2
//     |> List.map calcDist
//     |> List.min

let rec findPosInList list target steps =
    match list with
    | [] -> -1
    | head::tail ->
        if target = head then steps
        else findPosInList tail target (steps - 1)

let findCombinedPos list1 list2 target =
    let pos1 = findPosInList list1 target list1.Length
    let pos2 = findPosInList list2 target list2.Length
    pos1 + pos2

let UpgB =
    let inputFile = "./AoC2019/Day3/input.txt"
    let commands = getCommands inputFile
    let l1 = doCommands commands.[0] emptyList
    let l2 = doCommands commands.[1] emptyList
    filterLists l1 l2
    |> List.map (fun e -> findCombinedPos l1 l2 e)
    |> List.map (fun e -> e - 2)
    |> List.filter (fun e -> e <> 0)
    |> List.min