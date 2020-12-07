#load "./utils.fsx"

open AoCUtil

type Bag = { Color: string; Number: int; Content: Bag list;  }

let testInput1 =
    [
        "light red bags contain 1 bright white bag, 2 muted yellow bags.";
        "dark orange bags contain 3 bright white bags, 4 muted yellow bags.";
        "bright white bags contain 1 shiny gold bag.";
        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.";
        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.";
        "dark olive bags contain 3 faded blue bags, 4 dotted black bags.";
        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.";
        "faded blue bags contain no other bags.";
        "dotted black bags contain no other bags."
    ]

let testInput2 =
    [
        "shiny gold bags contain 2 dark red bags.";
        "dark red bags contain 2 dark orange bags.";
        "dark orange bags contain 2 dark yellow bags.";
        "dark yellow bags contain 2 dark green bags.";
        "dark green bags contain 2 dark blue bags.";
        "dark blue bags contain 2 dark violet bags.";
        "dark violet bags contain no other bags."
    ]


let strToBag (str: string) =
    let pColor = str.Substring(0, str.IndexOf(" bags contain"))
    let contents = if str.Contains("no other") then [] //{ Content = []; Color = str.Substring(0, str.IndexOf("bags")); Number = 0}
                   else (List.map ((fun (e: string) -> 
                        e.Substring(0, e.IndexOf("bag"))) 
                        >> ((fun (e: string) -> (int (e.Substring(0, 2)), e.Substring(3, e.Length-4))) 
                        >> (fun (x, y) -> {Content=[]; Color=y;Number=x})))
                        (Array.toList (str.Substring(pColor.Length + 13, str.Length - (pColor.Length + 13)).Split(","))))
    { Content = contents; Color= pColor; Number=1}

let canHold str bag =
    if List.isEmpty bag.Content then false
    else bag.Content |> List.map (fun e -> e.Color = str) |> List.reduce (||)

let getPossible str bags  = 
    bags
    |> List.filter(fun e -> canHold str e)

let rec addItemsToSet (set: string Set) (list: string List) =
    match list with
    | [] -> set
    | hd::tl -> addItemsToSet (set.Add hd) tl

let rec concatLists l1 l2 =
    match l2 with
    | [] -> l1
    | hd::tl -> concatLists (l1 @[hd]) tl

let rec countPossible (str: string list) bags (possible: string Set) =
    if str.Length = 0 then possible
    else 
        let res = getPossible str.[0] bags |> List.map (fun e -> e.Color)
        let newStr = concatLists str res
        let newPos = addItemsToSet possible res
        match newStr with
        | [] -> possible
        | _::tl -> countPossible tl bags newPos
    

let getChildren (allBags: Bag list) str =
    let res = allBags
            |> List.reduce(fun e i -> if e.Color = str then e else i)
    res.Content        

let rec countChildren allBags (bag: Bag) =
    let children = getChildren allBags bag.Color
    if List.isEmpty children
    then
        printfn "%s bags contain 0 other bags." bag.Color
        1
    else 
        let x = 1 + List.sumBy (fun e -> (e.Number * countChildren allBags e)) children
        printfn "%s bags contain %i other bags" bag.Color x
        x


let example1 =
    let input = testInput1
                |> List.map(strToBag)
    let firstBag = input |> List.reduce(fun e i -> if e.Color = "shiny gold" then e else i)
    printfn "firstBag: %A" firstBag
    let res = countChildren input firstBag
    res - 1


let example2 =
    let input = testInput2
                |> List.map(strToBag)
    let firstBag = input |> List.reduce(fun e i -> if e.Color = "shiny gold" then e else i)
    printfn "firstBag: %A" firstBag
    let res = countChildren input firstBag
    res - 1

let a =
    let input = readLines "./input7.txt"
                |> List.map(strToBag)
    let res = countPossible ["shiny gold"] input Set.empty
    res.Count   

let b =
    let input =  readLines "./input7.txt"
                |> List.map(strToBag)
    let firstBag = input |> List.reduce(fun e i -> if e.Color = "shiny gold" then e else i)
    printfn "firstBag: %A" firstBag
    let res = countChildren input firstBag
    res - 1