open System.Text.RegularExpressions
#load "./utils.fsx"

open AoCUtil

type KvP = { Key: string; Value: string }

let debug = [
                "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"; "";
                "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"; "";
                "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"; "";
                "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"; "";
            ]

let rec fix (content: string list) pos result =
    if content.Length <= pos
    then (result, pos + 1)
    else if content.[pos] = ""
    then (result, pos + 1)
    else fix content (pos+1) (result + " " + content.[pos])

let rec fixAll (content: string list) pos result =
    let (res, newPos) = fix content pos ""
    let newRes = if res <> "" then result @[res] else result
    if content.Length <= newPos
    then newRes
    else fixAll content (newPos) newRes

let input = readLines "./AoC2020/input4.txt"
            |> List.map(fun e -> e.Replace("\n", ""))

let fixedInput = fixAll input 0 []

let parseLineIntoKvp (str: string) =
    str.Split(" ")
    |> Array.map(fun e ->
        // printf "%s \n" e
        match e.IndexOf(":") with
        | -1 -> None
        | _ -> Some { Key = e.Split(":").[0]; Value = e.Split(":").[1] })
    |> Array.toList

let hasReqField line req eval =
    line
    |> List.map(fun e -> e.Key = req && (eval e.Value))
    |> List.reduce( || )

let noValidation s =
    true

let byrValid s =
    let v = int s
    v >= 1920 && v <= 2002

let iyrValid s =
    let v = int s
    v >= 2010 && v <= 2020

let eyrValid s =
    let v = int s
    v >= 2020 && v <= 2030

let hgtValid (s:string) =
    if s.Length <= 2 then false
    else 
        let unitType = s.Substring(s.Length - 2, 2)
        let len = int (s.Substring(0, s.Length - 2))
        match unitType with
        | "cm" -> len >= 150 && len <= 193
        | "in" -> len >= 59 && len <= 76
        | _ -> false

let hclValid (s: string) =
    let pattern = Regex "[#][a-f0-9]{6}"
    pattern.IsMatch s 

let eclValid s =
    match s with
    | "amb" -> true
    | "blu" -> true
    | "brn" -> true
    | "gry" -> true
    | "grn" -> true
    | "hzl" -> true
    | "oth" -> true
    | _ -> false

let pidValid (s: string) =
    if s.Length = 9 then
        let pattern = Regex "[0-9]{9}"
        pattern.IsMatch s
    else false

let isPassportValid str =
    let parsed = parseLineIntoKvp str |> List.choose (id)
    if parsed.Length = 0
    then false
    else
        [
            hasReqField parsed "byr" noValidation;
            hasReqField parsed "iyr" noValidation;
            hasReqField parsed "eyr" noValidation;
            hasReqField parsed "hgt" noValidation;
            hasReqField parsed "hcl" noValidation;
            hasReqField parsed "ecl" noValidation;
            hasReqField parsed "pid" noValidation;
        ]
        |> List.reduce( && )

let isPassportValid2 str =
    let parsed = parseLineIntoKvp str |> List.choose (id)
    if parsed.Length = 0 then false
    else
        [
            hasReqField parsed "byr" byrValid;
            hasReqField parsed "iyr" iyrValid;
            hasReqField parsed "eyr" eyrValid;
            hasReqField parsed "hgt" hgtValid;
            hasReqField parsed "hcl" hclValid;
            hasReqField parsed "ecl" eclValid;
            hasReqField parsed "pid" pidValid;
        ]
        |> List.reduce( && )

let a = 
    fixedInput
    |> List.map(isPassportValid)
    |> List.filter(id)
    |> List.length

let b =
    fixedInput
    |> List.map(isPassportValid2)
    |> List.filter(id)
    |> List.length
