module Day8
open AoCUtil

type Node = { Left: string; Right: string }

let test = [
    "LLR"
    ""
    "AAA = (BBB, BBB)"
    "BBB = (AAA, ZZZ)"
    "ZZZ = (ZZZ, ZZZ)"
]

let test2 = [
    "LR"
    ""
    "11A = (11B, XXX)"
    "11B = (XXX, 11Z)"
    "11Z = (11B, XXX)"
    "22A = (22B, XXX)"
    "22B = (22C, 22C)"
    "22C = (22Z, 22Z)"
    "22Z = (22B, 22B)"
    "XXX = (XXX, XXX)"
]

let path (strList: string list) =
    strList.[0] |> Seq.toList

let nodes (strList: string list) =
    let parseNode (str: string) =
        let split = str.Replace("(", "").Replace(")", "").Split(" = ")
        let nodes = split.[1].Split(",")
        (split.[0], { Left = nodes.[0].Trim(); Right = nodes.[1].Trim()})
    let rec parseAllNodes acc (strList: string list) =
        match strList with
        | [] -> acc
        | hd::tl -> parseAllNodes ((parseNode hd)::acc) tl
    parseAllNodes [] (strList |> List.skip 2) |> List.rev |> Map

let move (nodeMap: Map<string,Node>) turn key =
    if turn = 'L' then nodeMap.[key].Left
    else nodeMap.[key].Right

let doAllMoves nodeMap moveList pos isTarget =
    let rec doAllMovesRec (acc: int) (nodeMap: Map<string,Node>) moveList pos =
        match moveList with
        | [] -> (pos, acc)
        | hd::tl -> (
                        let newPos = move nodeMap hd pos
                        if isTarget newPos then (newPos, acc)
                        else doAllMovesRec (acc+1) nodeMap tl newPos
                    )
    let rec doUntilDone (acc: int) (nodeMap: Map<string,Node>) moveList pos =
        let (p, a) = doAllMovesRec acc nodeMap moveList pos
        if isTarget p then a
        else doUntilDone a nodeMap moveList p 
    doUntilDone 1 nodeMap moveList pos

let target1 str =
    str = "ZZZ"

let target2 (str: string) =
    str.[2] = 'Z'

let day8A =
    let input = test
    // let input = readLines @".\input.txt"
    let p = path input
    let n = nodes input
    doAllMoves n p "AAA" target1

let day8B =
    // let input = test2
    let input = readLines @".\input.txt"
    let p = path input
    let n = nodes input
    n.Keys 
        |> Seq.where(fun str -> str.[2] = 'A') |> Seq.map(fun f -> doAllMoves n p f target2) 
        |> Seq.toList
        |> MGM
    