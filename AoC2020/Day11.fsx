#load "./utils.fsx"
open AoCUtil

let testInput = [
                    "L.LL.LL.LL";
                    "LLLLLLL.LL";
                    "L.L.L..L..";
                    "LLLL.LL.LL";
                    "L.LL.LL.LL";
                    "L.LLLLL.LL";
                    "..L.L.....";
                    "LLLLLLLLLL";
                    "L.LLLLLL.L";
                    "L.LLLLL.LL"
                ]


let getNeighbours (input: string list) x y =
    let innerHelper (input: string list) a b =
        if (a >= 0 && a < input.[0].Length) && (b >= 0 && b < input.Length)
        then Some input.[b].[a]
        else None
    [
        innerHelper input (x+1) y;
        innerHelper input (x-1) y;
        innerHelper input (x+1) (y+1);
        innerHelper input (x-1) (y+1);
        innerHelper input (x+1) (y-1);
        innerHelper input (x-1) (y-1);
        innerHelper input x (y+1);
        innerHelper input x (y-1)
    ] |> List.choose(id)
    
let getVisible (input: string list) x y =
    let rec innerHelper (input: string list) a b f =
        if (a >= 0 && a < input.[0].Length) && (b >= 0 && b < input.Length) then
            if input.[b].[a] = '.'
            then 
                let (nA, nB) = f a b
                innerHelper input nA nB f
            else Some input.[b].[a]
        else None
    [
        innerHelper input (x+1) y (fun a b -> (a+1, b));
        innerHelper input (x-1) y (fun a b -> (a-1, b));
        innerHelper input (x+1) (y+1) (fun a b -> (a+1, b+1));
        innerHelper input (x-1) (y+1) (fun a b -> (a-1, b+1));
        innerHelper input (x+1) (y-1) (fun a b -> (a+1, b-1));
        innerHelper input (x-1) (y-1) (fun a b -> (a-1, b-1));
        innerHelper input x (y+1) (fun a b -> (a, b+1));
        innerHelper input x (y-1) (fun a b -> (a, b-1))
    ] |> List.choose(id)

let shouldBeOccupied x y f (input: string list) =
    let t = input.[y].[x]
    if t = '#' then false
    else if t = '.' then false
    else
        let n = List.sumBy (fun e -> if e = '#' then 1 else 0) (f input x y)
        n = 0
                       
let shouldBeEmpty x y c f (input: string list) =
    let t = input.[y].[x]
    if t = 'L' then false
    else if t = '.' then false
    else
        let n = List.sumBy (fun e -> if e = '#' then 1 else 0) (f input x y)
        n >= c

let rec applyX x y c f (input: string list) =
    if x >= input.[0].Length then []
    else
        if shouldBeOccupied x y f input
        then ['#'] @ applyX (x+1) y c f input
        else if shouldBeEmpty x y c f input
        then ['L'] @ applyX (x+1) y c f input
        else
            [input.[y].[x]] @ applyX (x+1) y c f input

let rec applyY x y c f (input: string list) =
    if y >= input.Length then []
    else
        let charList = applyX x y c f input
        let str = System.String.Concat(Array.ofList(charList))
        [str]@(applyY x (y+1) c f input)

let rec applyUntilNoChange c f input =
    let newInput = applyY 0 0 c f input
    if newInput = input then input
    else applyUntilNoChange c f newInput

let countOccupiedSeats (input: string list) =
    List.sumBy (fun e -> 
            e |> Seq.filter(fun f -> 
                f = '#') |> Seq.length) input

let solveA input =
    input
        |> applyUntilNoChange 4 getNeighbours
        |> countOccupiedSeats

let solveB input =
    input
     |> applyUntilNoChange 5 getVisible
     |> countOccupiedSeats

let exampleA1 =
    solveA testInput 

let a = 
    solveA (readLines "./input11.txt") 

let exampleB =
    solveB testInput
        
let b =
    solveB (readLines "./input11.txt") 