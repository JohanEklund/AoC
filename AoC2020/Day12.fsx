#load "./utils.fsx"
open AoCUtil


type Heading = East | West | North | South
type Position = {X: int; Y: int}
type Ship = { Position: Position; Heading: Heading }

let exampleInput =
    [
        "F10";
        "N3";
        "F7";
        "R90";
        "F11"
    ]

let headingToInt h =
    match h with
    | East -> 0
    | West -> 180
    | North -> 90
    | South -> 270

let intToHeading i =
    match (i + 360) % 360 with
    | 0 -> East
    | 90 -> North
    | 180 -> West
    | 270 -> South
    | _ ->
        printfn "Shit happend!"
        East

let newHeading current angle direction =
    if direction = "R" then
         let a = headingToInt current
         intToHeading (a - angle)
    else 
        let a = headingToInt current
        intToHeading (a + angle)

let turn s a d =
    let n = newHeading s.Heading a d
    { Position = s.Position; Heading = n }

let move s d v =
    match d with
    | "N" -> { Position = { X = s.Position.X; Y = s.Position.Y + v }; Heading = s.Heading}
    | "S" -> { Position = { X = s.Position.X; Y = s.Position.Y  - v }; Heading = s.Heading}
    | "E" -> { Position = { X = s.Position.X + v; Y = s.Position.Y }; Heading = s.Heading}
    | "W" -> { Position = { X = s.Position.X - v; Y = s.Position.Y }; Heading = s.Heading}
    | _ ->  printfn "move bad" 
            s
let moveForward s v =
    match s.Heading with
    | East -> { Position = { X = s.Position.X + v; Y = s.Position.Y }; Heading = s.Heading}
    | West -> { Position = { X = s.Position.X - v; Y = s.Position.Y }; Heading = s.Heading}
    | North -> { Position = { X = s.Position.X; Y = s.Position.Y + v }; Heading = s.Heading}
    | South -> { Position = { X = s.Position.X; Y = s.Position.Y - v }; Heading = s.Heading}

let performInstr ship (instr: string) =
    let first = instr.[0]
    let rest = int (instr.Substring(1, (instr.Length - 1)))
    match first with
    | 'N' -> move ship "N" rest
    | 'S' -> move ship "S" rest
    | 'E' -> move ship "E" rest
    | 'W' -> move ship "W" rest
    | 'L' -> turn ship rest "L"
    | 'R' -> turn ship rest "R"
    | 'F' -> moveForward ship rest
    | _ -> 
        printfn "perform BAD"
        ship

let rec performAllInstr ship (ins: string list) =
    match ins with
    | [] -> ship
    | hd::tl -> performAllInstr (performInstr ship hd) tl

let calcManhattanDist ship =
    (abs ship.Position.X) + (abs ship.Position.Y)

let startingShip = { Position = { X = 0; Y = 0}; Heading = East}

let example1 =
    performAllInstr startingShip exampleInput
        |> calcManhattanDist

let a = 
    performAllInstr startingShip (readLines "./input12.txt") 
        |> calcManhattanDist