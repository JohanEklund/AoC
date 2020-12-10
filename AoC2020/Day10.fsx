#load "./utils.fsx"
open AoCUtil

let testInput1 = [ 16; 10; 15; 5; 1; 11; 7; 19; 6; 12; 4; ]
let testInput2 = [28; 33; 18; 42; 31; 14; 46; 20; 48; 47; 24; 23; 49; 45; 19; 38; 39; 11; 1; 32; 25; 35; 8; 17; 7; 9; 4; 2; 34; 10; 3;]

let addChargingAndBuiltIn list =
    let m = List.max list
    list @[0; (m+3)]

let createAdapterPairs sortedList =
    sortedList
        |> List.mapi(fun i e ->
                        if (i + 1) >= sortedList.Length then None
                        else Some(e, sortedList.[(i + 1)])
                    )
        |> List.choose(id)

let calcDiff a =
    let (x, y) = a
    y - x

let calcRes a =
    let diff1 = a |> List.filter(fun e -> e = 1) |> List.length
    let diff3 = a |> List.filter(fun e -> e = 3) |> List.length
    diff1 * diff3

let countPossibilities i (list: int list) =
    if i+3 >= list.Length then (0, []) 
    else
        let x = list.[i]
        let p = [
                    if list.[i+1] - x > 3 then None else Some (i+1);
                    if list.[i+2] - x > 3 then None else Some (i+2);
                    if list.[i+3] - x > 3 then None else Some (i+3);
                ]
                |> List.choose(id)
        (i, p)

let solveA input =
    input
        |> addChargingAndBuiltIn
        |> List.sort
        |> createAdapterPairs
        |> List.map(calcDiff)
        |> calcRes

let exampleA1 =
    solveA testInput1
                     
let exampleA2 =
    solveA testInput2

let a =
    solveA (readIntLines "./AoC2020/input10.txt")
