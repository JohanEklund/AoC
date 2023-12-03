module Day3

open System
open AoCUtil

type Position = { X : int; Y : int }
type Schematic = { Position : Position; Part : string }
type Symbol = { Position: Position; Type : char}

let test = [
    "467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."
]

let parseParts (str: string) (y: int) =
    str 
        |> Seq.fold (fun (t, a, x) v -> 
            if Char.IsNumber(v) 
            then
                if t then (true, { Position = a.Head.Position; Part = $"{a.Head.Part}{v}"}::a.Tail, x+1) 
                else (true, { Position = { X = x; Y = y}; Part = $"{v}"}::a, x+1)
            else (false, a, x+1)) (false, [], 0)
        |> fun (_, res, _) -> res

let getSymbolPos (str: string) =
    str
        |> Seq.mapi(fun i c -> 
        if c = '.' || Char.IsNumber(c) 
        then None 
        else Some (i, c))
        |> Seq.fold(fun a v -> match v with
                                | Some x -> x::a
                                | None -> a) []
        |> Seq.rev

let parseSymbols (input : list<string>) =        
    let rec parseRec acc str =
        match str with
        | hd::tl -> parseRec ((getSymbolPos hd)::acc) tl
        | [] -> acc
    parseRec [] input
    |> Seq.rev
    |> Seq.mapi (fun i f -> f |> Seq.fold(fun a (x, c) -> { Position = { X = x; Y = i}; Type = c}::a) [])
    |> Seq.where (fun f -> f <> [])
    |> Seq.collect (fun f -> f)

let isNeighbour a b l =
    if (Math.Abs (a.Y - b.Y)) > 1 then false
    else if (a.X >= b.X - 1 && a.X <= b.X + l) then true
    else false

let rec isNeighbourRec a b length =
    match a with
    | [] -> false
    | hd::tl -> if isNeighbour hd b length then true else isNeighbourRec tl b length

let getNeighbourParts (schematics: Schematic list) (symbols: Symbol list) =
    let symbolPos = symbols |> List.map(fun f -> f.Position)
    schematics
        |> Seq.where(fun f -> isNeighbourRec symbolPos f.Position f.Part.Length)

let getSymbolNeighbours (schematics: Schematic list) (symbol: Symbol) =
    schematics 
        |> List.where (fun f -> isNeighbour symbol.Position f.Position f.Part.Length)
    
let day3A =
    let input = readLines @".\input.txt" 
    // let input = test
    let symbols = parseSymbols input |> Seq.toList
    let parts = input
                |> Seq.mapi(fun i s -> parseParts s i)
                |> Seq.collect (fun f -> f)
                |> Seq.toList
    getNeighbourParts parts symbols
        |> Seq.map(fun f -> f.Part |> int)
        |> Seq.sum

let day3B =
    // let input = test
    let input = readLines @".\input.txt" 
    let gears = parseSymbols input |> Seq.where(fun x -> x.Type = '*')
    let parts = input
                |> Seq.mapi(fun i s -> parseParts s i)
                |> Seq.collect (fun f -> f)
                |> Seq.toList
    gears
        |> Seq.map(fun f -> getSymbolNeighbours parts f)
        |> Seq.where(fun f -> f.Length = 2)
        |> Seq.map(fun f -> (f.[0].Part |> int) * (f.[1].Part |> int))
        |> Seq.sum
                
    

