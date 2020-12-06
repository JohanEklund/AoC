#load "./utils.fsx"

open AoCUtil

let debug = [
    "abc";
    "";
    "a";
    "b";
    "c";
    "";
    "ab";
    "ac";
    "";
    "a";
    "a";
    "a";
    "a";
    "";
    "b";
]


let rec parseGroup li res =
    match li with
    | [] -> (li, res)
    | hd :: tl -> 
        if hd = "" then (tl, res)
        else parseGroup tl (res + hd)

let rec parseAll li res =
    let (newLi, newRes) = parseGroup li ""
    match newLi with
    | [] -> res @[newRes]
    | l -> parseAll l (res @[newRes])


let a = List.sumBy (fun (e: char Set) -> e.Count) (parseAll (readLines "./AoC2020/input6.txt") []
        |> List.map(Set.ofSeq))


let rec parseGroup2 li res =
    match li with
    | [] -> (li, res)
    | hd :: tl -> 
        if hd = "" then (tl, res)
        else parseGroup2 tl (res @[hd])

let rec parseAll2 li res =
    let (newLi, newRes) = parseGroup2 li []
    match newLi with
    | [] -> res @[newRes]
    | l -> parseAll2 l (res @[newRes])

let matchingCharacters (x: string) (y: string) =
    if x = y then x
    else
        let setX = Set.ofSeq x
        let setY = Set.ofSeq y
        Set.intersect setX setY
            |> Set.toSeq
            |> Seq.toArray
            |> System.String
            
let rec countWhereAll (li: string list) =
    if li.Length = 1 then li.[0].Length
    else
        let newItem = matchingCharacters li.[0] li.[1]
        match li with
        | [] -> 0
        | _::_::tl2 -> countWhereAll (tl2 @[newItem])

let b = List.sumBy countWhereAll (parseAll2 (readLines "./AoC2020/input6.txt") [])

