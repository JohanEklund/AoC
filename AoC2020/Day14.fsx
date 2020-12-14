open System
#load "./utils.fsx"
open AoCUtil

type MaskBit = One | Zero | X
type Mem = { Index: int; Value: int }
// type Mask = { M: MaskBit list }
type Op = 
    | Mem of Mem
    | Mask of MaskBit list

let exampleInput = [
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X";
    "mem[8] = 11";
    "mem[7] = 101"
    "mem[8] = 0"
]

let parseMask (s: string) =
    printfn "mask %s" s
    s.Substring(7, 36)
            |> Seq.toList
            |> List.map(fun e -> 
                        match e with
                        | 'X' -> X
                        | '1' -> One
                        | '0' -> Zero
                        | _ -> 
                                printfn "parseMask bad!"
                                X)
    

let parseMem (str: string) =
    printfn "mem %s" str
    let last = str.IndexOf("]")
    let pointer = str.Substring(4, last - 4)
    let value = str.Substring(last + 3, str.Length - (last + 3))
    (int pointer, int value)

let parse (s: string list) = 
    s
        |> List.map(fun e ->
                        if e.[1] = 'a' then 
                            printfn "mask %s" e
                            parseMask e |> Mask
                        else 
                            printfn "mem %s" e
                            let (i, v) = parseMem e
                            Mem{Index = i; Value = v}
                    )

let toBit (x: int) =
    let s = Convert.ToString(x, 2)
    s.PadLeft(36, '0')

let toInt64 (x: string) =
    let rec inner s r i =
        match s with
        | [] -> r
        | (hd::tl) -> 
                    let x = int64 (Char.ToString hd)
                    inner tl (r + (x * (pown 2L i))) (i-1)
    inner (x |> Seq.toList) 0L 35

let applyOne (v: string) pos =
    let first = v.[0 .. pos]
    let second = v.[pos+1 .. v.Length-1]
    first + "1" + second

let applyMaskToPos m op (arr: int64 list) =
    let rec inner m (b: string) i =
        match m with
        | [] -> b
        | (hd::tl) -> 
            printfn "Head: %A Tail: %A" hd tl
            match hd with
            | One ->
                    let newB = b.[0 .. i-1] + "1" + b.[i+1 .. b.Length - 1]
                    printfn "One at %i: %s -> %s" i b newB
                    inner tl newB (i+1)
            | Zero -> 
                    let newB = b.[0 .. i-1] + "0" + b.[i+1 .. b.Length - 1]
                    printfn "One at %i: %s -> %s" i b newB
                    inner tl newB (i+1)
            | X -> inner tl b (i+1)
    let b = toBit op.Value
    let res = inner m b 0
    arr |> List.mapi(fun i e -> if i = op.Index then toInt64 res else e)
    

let rec handle l m (v: int64 list) =
    match l with
    | [] -> v
    | (hd::tl) -> 
            match hd with
            | Mask (x) -> handle tl x v
            | Mem (x) ->
                    printfn "apply %A to %A" m x    
                    let newV = applyMaskToPos m x v
                    printfn "result: %A" newV
                    handle tl m newV

let initArr size =
    [ for i in 1 .. size -> int64 0]

let exapleA =
    let input = parse exampleInput
    handle input [] (initArr 10)
        |> List.sum


let a =
    let lines = readLines "./input14.txt"
    let input = parse lines
    handle input [] (initArr 99000)
        |> List.sum