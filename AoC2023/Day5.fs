module Day5

open System;
open AoCUtil

type Range = { Start : int64; Length : int64 }

let test = [
    "seeds: 79 14 55 13"
    ""
    "seed-to-soil map:"
    "50 98 2"
    "52 50 48"
    ""
    "soil-to-fertilizer map:"
    "0 15 37"
    "37 52 2"
    "39 0 15"
    ""
    "fertilizer-to-water map:"
    "49 53 8"
    "0 11 42"
    "42 0 7"
    "57 7 4"
    ""
    "water-to-light map:"
    "88 18 7"
    "18 25 70"
    ""
    "light-to-temperature map:"
    "45 77 23"
    "81 45 19"
    "68 64 13"
    ""
    "temperature-to-humidity map:"
    "0 69 1"
    "1 0 69"
    ""
    "humidity-to-location map:"
    "60 56 37"
    "56 93 4"
]

let seedToSoil = "seed-to-soil map:"
let soilToFert = "soil-to-fertilizer map:"
let fertToWater = "fertilizer-to-water map:"
let waterToLight = "water-to-light map:"
let lightToTemp = "light-to-temperature map:"
let tempToHum = "temperature-to-humidity map:"
let humToLoc = "humidity-to-location map:"

let parseSeeds (str: string) =
    str.Split(" ") 
        |> Seq.skip 1
        |> Seq.map(fun f -> f |> int64)

let parseSeeds2 str =
    let rec seedRangeRec acc list =
        match list with
        | [] -> acc
        | a::b::tl -> seedRangeRec ({Start = a; Length = b}::acc) tl
        | _ -> failwith "odd number of seeds"
    let seeds = parseSeeds str |> Seq.toList
    seedRangeRec [] seeds
        

let parseMap title list =
    let rec parseMapRec acc (list: string list) =
        if list = [] || list.[0] = "" then acc
        else
            match list with
            | [] -> acc
            | hd::tl -> parseMapRec ((hd.Split(" ") |> Seq.map(fun f -> f |> int64) |> Seq.toList)::acc) tl
    let rec buildMapRec acc dest source inc =
        if inc = 0L then acc
        else
            buildMapRec ((source, dest)::acc) (dest+1L) (source+1L) (inc-1L)
    let start = list |> List.findIndex(fun f -> f = title)
    let x = parseMapRec [] list[start+1..]
    // x |> List.fold (fun a b -> buildMapRec a b.[0] b.[1] b.[2]) [] |> Map
    x |> List.fold (fun a b -> ({ Start = b.[1]; Length = b.[2]}, { Start = b.[0]; Length = b.[2]})::a) []

let translate (map: List<(Range * Range)>) (key: int64) =
    let x = map |> List.where(fun (l, _) -> key >= l.Start && (l.Start + l.Length) > key)
    if x = [] then key
    else if x.Length > 1 then failwith "matched more than one" 
    else 
        let (a, b) = x.[0]
        let offset = key - a.Start
        b.Start + offset
        
let input = readLines @".\input.txt" 

let seeds = parseSeeds input.[0]
let seedMap = parseMap seedToSoil input
let soilMap = parseMap soilToFert input
let fertMap = parseMap fertToWater input
let waterMap = parseMap waterToLight input
let lightMap = parseMap lightToTemp input
let tempMap = parseMap tempToHum input
let humMap = parseMap humToLoc input

let doTheThing seeds =
    seeds 
        |> Seq.map(fun f -> (translate seedMap) f)
        |> Seq.map(fun f -> (translate soilMap) f)
        |> Seq.map(fun f -> (translate fertMap) f)
        |> Seq.map(fun f -> (translate waterMap) f)
        |> Seq.map(fun f -> (translate lightMap) f)
        |> Seq.map(fun f -> (translate tempMap) f)
        |> Seq.map(fun f -> (translate humMap) f)

let doTheThing2 seed =
    seed
        |> translate seedMap
        |> translate soilMap
        |> translate fertMap
        |> translate waterMap
        |> translate lightMap
        |> translate tempMap
        |> translate humMap

let loopOverRange range =
    printf $"Looping over range starting at {range.Start} with length {range.Length}{Environment.NewLine}"
    let rec loopOverRangeRec range i (acc: int64) =
        let ten = range.Length / 10L
        if i = ten then printf $"10{Environment.NewLine}"
        else if i = 2L * ten then printf $"20{Environment.NewLine}"
        else if i = 3L * ten then printf $"30{Environment.NewLine}"
        else if i = 4L * ten then printf $"40{Environment.NewLine}"
        else if i = 5L * ten then printf $"50{Environment.NewLine}"
        else if i = 6L * ten then printf $"60{Environment.NewLine}"
        else if i = 7L * ten then printf $"70{Environment.NewLine}"
        else if i = 8L * ten then printf $"80{Environment.NewLine}"
        else if i = 9L * ten then printf $"90{Environment.NewLine}"
        if i = (range.Length) then acc
        else
            let a = doTheThing2 (range.Start+i)
            loopOverRangeRec range (i+1L) (Math.Min(a, acc))
    loopOverRangeRec range 0 Int64.MaxValue


let day5A =
    doTheThing seeds |> Seq.min

let day5B =
    parseSeeds2 input.[0]
        |> List.map(fun f -> loopOverRange f)
        |> Seq.min
        
        