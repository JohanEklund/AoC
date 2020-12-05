#load "./utils.fsx"

open AoCUtil

type Range = { Min: int; Max: int }
type Seat = { Col: int; Row: int }

let parse (s: string) =
    let x = s.Substring(0, 7)
    let y = s.Substring(7, 3)
    (Seq.toList x, Seq.toList y)

let calcRow c i =
    match c with
    | 'F' -> Some { Min = i.Min; Max = (i.Max + i.Min) / 2 }
    | 'L' -> Some { Min = i.Min; Max = (i.Max + i.Min) / 2 }
    | 'B' -> Some { Min = (i.Min + i.Max) / 2 + 1; Max = i.Max }
    | 'R' -> Some { Min = (i.Min + i.Max) / 2 + 1; Max = i.Max }
    | _ -> None

let rec loopRow (fb: char list) range i =
    if i = fb.Length then range
    else
        let newRange = calcRow fb.[i] range
        match newRange with
        | Some r -> loopRow fb r (i + 1)
        | None -> { Min = -1; Max = -1 }

let rangeToSeat r c =
    if r.Min = r.Max && c.Min = c.Max
    then Some { Col = c.Min; Row = r.Min }
    else 
        printfn "BAD %A %A" r c
        None

let calcSeatId (seat: Seat Option) =
    match seat with
    | Some s -> s.Row * 8 + s.Col
    | None -> -1
    

let stringToSeat s =
    let (rows, cols) = parse s
    let x = loopRow rows { Min = 0; Max = 127 } 0
    let y = loopRow cols { Min = 0; Max = 7 } 0
    rangeToSeat x y

let debugValues = [
    "FBFBBFFRLR"; // row 44, column 5, seat ID 357
    "BFFFBBFRRR"; // row 70, column 7, seat ID 567
    "FFFBBBFRRR"; // row 14, column 7, seat ID 119
    "BBFFBBFRLL"  // row 102, column 4, seat ID 820
]

let debug =
    List.map ((stringToSeat) >> (calcSeatId)) debugValues

let a =
    List.map ((stringToSeat) >> (calcSeatId)) (readLines "./AoC2020/input5.txt")
    |> List.max

let rec generateAllPossible res cur =
    let newSeat = calcSeatId (Some cur)
    if cur.Col < 7 then generateAllPossible (res @[newSeat]) {Col = cur.Col + 1; Row = cur.Row}
    else if cur.Row < 126 then generateAllPossible (res @[newSeat]) {Col = 0; Row = cur.Row + 1}
    else res

let evalCandidate i (existingSeats: int Set) =
    existingSeats.Contains(i+1) && existingSeats.Contains(i-1)

let b =
    let existing = List.map ((stringToSeat) >> (calcSeatId)) (readLines "./AoC2020/input5.txt")
                    |> Set.ofList
    let all = generateAllPossible [] { Col = 0; Row = 1 }
                    |> Set.ofList
    let res = Set.difference all existing
                    |> Set.filter(fun e -> evalCandidate e existing)
    (Set.toList res).[0]