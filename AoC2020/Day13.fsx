open System.Threading.Tasks
#load "./utils.fsx"
open AoCUtil

let exampleInput = ["939"; "7,13,x,x,59,x,31,19"]

let parseIds (str: string) =
    let s = str.Split(',') 
            |> Seq.filter(fun e -> e <> "x")
            |> Seq.map(int)
            |> Seq.toList
    s

let departingBusses min busses =
    busses  
        |> List.filter(fun e -> (min % e = 0))

let rec findDeparture m b =
    let departuresNow = departingBusses m b
    match departuresNow with
    | [] -> findDeparture (m+1) b
    | (id::_) -> (id, m)

let exampleA =
    let start = int exampleInput.[0]
    let ids = parseIds exampleInput.[1]
    let (id, w) = findDeparture start (ids)
    (w - start) * id
   
let a =
    let input = readLines "./AoC2020/input13.txt"
    let start = int input.[0]
    let ids = parseIds input.[1]
    let (id, w) = findDeparture start (ids)
    (w - start) * id

let finall =
    let rec inner x =
        if ((x+19L) % 787L = 0L && (x+50L) % 571L = 0L && (x+9L) % 41L = 0L && (x+13L) % 37L = 0L && (x+48L) % 29L = 0L && (x+42L) % 23L = 0L && x%19L = 0L && (x+67L) % 17L = 0L && (x+32L) % 13L = 0L) then x
        else inner (x + 787L)
    inner 768L