module Day10

open AoCUtil
open System

let test = [
    "..F7."
    ".FJ|."
    "SJ.L7"
    "|F--J"
    "LJ..."
]

// let input = test
let input = readLines @".\input.txt"

let isConnected (grid: string list) pos newPos =
    let (newX, newY) = newPos
    if newX < 0 || newY < 0 || newY >= grid.Length || newX >= grid.[newY].Length then false
    else
        let (oldX, oldY) = pos
        let diff = (oldX-newX, oldY-newY)
        match diff with
        | (x,y) when x = -1 && y = 0 ->  (grid.[oldY].[oldX] = 'S' || grid.[oldY].[oldX] = '-' || grid.[oldY].[oldX] = 'L' || grid.[oldY].[oldX] = 'F') && (grid.[newY].[newX] = '-' || grid.[newY].[newX] = 'J' || grid.[newY].[newX] = '7')
        | (x,y) when x = 1 && y = 0 -> (grid.[oldY].[oldX] = 'S' || grid.[oldY].[oldX] = '-' || grid.[oldY].[oldX] = 'J' || grid.[oldY].[oldX] = '7') && (grid.[newY].[newX] = '-' || grid.[newY].[newX] = 'L'|| grid.[newY].[newX] = 'F')
        | (x,y) when x = 0 && y = -1 -> (grid.[oldY].[oldX] = 'S' || grid.[oldY].[oldX] = '|' || grid.[oldY].[oldX] = '7' || grid.[oldY].[oldX] = 'F') && (grid.[newY].[newX] = '|' || grid.[newY].[newX] = 'L' || grid.[newY].[newX] = 'J')
        | (x, y) when x = 0 && y = 1 -> (grid.[oldY].[oldX] = 'S' || grid.[oldY].[oldX] = '|' || grid.[oldY].[oldX] = 'L' || grid.[oldY].[oldX] = 'J') && (grid.[newY].[newX] = '|' || grid.[newY].[newX] = '7' || grid.[newY].[newX] = 'F')
        | _ -> false

let isConnectedLogger grid pos newPos =
    let r = isConnected grid pos newPos
    // let (x, y) = newPos
    // let s = if r then "ARE" else "are NOT"
    // printfn $"{pos} and {newPos} {s} connected, character was {grid.[y].[x]}"
    r

let distanceGrid =
    [|for _ in 1..input.Length -> [|for _ in 1..input.[0].Length -> Int32.MaxValue|]|]

let findStart =
    let (_, x, y) = input |> List.fold(fun a b -> 
        let (isDone, x, y) = a
        if isDone then a
        else if b.Contains("S") then (true, b.IndexOf("S"), y)
        else (false, x, y+1)) (false, 0, 0)
    (x, y)

let updateGrid (grid: int array array) (pos: int*int) dis =
    let (x, y) = pos
    if grid.[y].[x] < dis then grid
    else
        grid.[y].SetValue(dis, x)
        grid

let moveOne grid (distanceGrid: int array array) pos =
    let (x, y) = pos
    let p1 = (x+1, y)
    let p2 = (x-1, y)
    let p3 = (x, y+1)
    let p4 = (x, y-1)
    let cur = distanceGrid.[y].[x]
    if x+1 < distanceGrid.[y].Length && distanceGrid.[y].[x+1] > cur+1 && isConnectedLogger grid pos p1 then Some (p1, updateGrid distanceGrid p1 (cur+1))
    else if x-1 >= 0 && distanceGrid.[y].[x-1] > cur+1 && isConnectedLogger grid pos p2 then Some (p2, updateGrid distanceGrid p2 (cur+1))
    else if y+1 < distanceGrid.Length && distanceGrid.[y+1].[x] > cur+1 && isConnectedLogger grid pos p3 then Some (p3, updateGrid distanceGrid p3 (cur+1))
    else if y-1 >= 0 && distanceGrid.[y-1].[x] > cur+1 && isConnectedLogger grid pos p4 then Some (p4, updateGrid distanceGrid p4 (cur+1))
    else None

let day10A =
    let rec loop dGrid pos1 pos2 =
        let a = moveOne input dGrid pos1
        match a with
        | None -> dGrid
        | Some (p1, g1) -> (
            let b = moveOne input g1 pos2
            match b with
            | None -> g1
            | Some (p2, g2) -> loop g2 p1 p2
        )
    let grid = distanceGrid
    let start = findStart
    let u = updateGrid grid start 0
    let (p1, m1) = (moveOne input  u start).Value
    let (p2, m2) = (moveOne input m1 start).Value
    loop m2 p1 p2
        |> Array.map(fun f -> f 
                            |> Array.where(fun x -> x < Int32.MaxValue))
        |> Array.collect id
        |> Array.max

let day10B = ""