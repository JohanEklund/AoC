#load "./utils.fsx"
open AoCUtil

type Requirement = { Low: int; High: int}

type Rule = { Name: string; Req1: Requirement; Req2: Requirement}

let parseRules (strs: string list) =
    strs |> List.map(fun e -> 
                    let name = e.Split(':').[0]
                    let r1 = e.Split(':').[1].Split(" or ").[0].Split('-')
                    let r2 = e.Split(':').[1].Split(" or ").[1].Split('-')
                    { Name = name; Req1 = { Low = int r1.[0]; High = int r1.[1]}; Req2 = { Low = int r2.[0]; High = int r2.[1]}})

let parse (input: string list) =
    printfn "parse!"
    let rec findBreaks l i p1=
        match l with
        | [] -> (0, 0)
        | (hd::tl) -> 
                    printfn "%s" hd
                    if hd = "" 
                    then 
                        if p1 <> 0 then (p1, i)
                        else findBreaks tl (i+1) i
                    else findBreaks tl (i+1) p1
    let (x, y) = findBreaks input 0 0
    printfn "found %i %i" x y
    let rules = input.[0 .. x-1] |> parseRules
    let ticket = input.[x+2].Split(',') |> Array.map(int) |> Array.toList
    let nearby = input.[y+2 .. input.Length-1] |> List.map(fun e ->
                                                            e.Split(',')
                                                            |> Array.map(int)
                                                            |> Array.toList)
    (rules, ticket, nearby)


let rec numberMatchesAnyRule n r =
    match r with
    | [] -> false
    | (hd::tl) -> 
                if (n >= hd.Req1.Low && n <= hd.Req1.High) || (n >= hd.Req2.Low && n <= hd.Req2.High)
                then true
                else numberMatchesAnyRule n tl

let ticketValid t r =
    t 
        |> List.filter(fun e -> not (numberMatchesAnyRule e r))
        |> List.sum

let exampleInput = [
    "class: 1-3 or 5-7";
    "row: 6-11 or 33-44";
    "seat: 13-40 or 45-50";
    "";
    "your ticket:";
    "7,1,14";
    "";
    "nearby tickets:";
    "7,3,47";
    "40,4,50";
    "55,2,20";
    "38,6,12";
]

let exampleA =
    let (r, _, t) = parse exampleInput
    List.sumBy (fun e -> ticketValid e r) t


let a =
    let (r, _, t) = parse (readLines ".\AoC2020\input16.txt")
    List.sumBy (fun e -> ticketValid e r) t