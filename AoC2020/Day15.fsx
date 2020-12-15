
let exampleInput1 = [0; 3; 6]
let exampleInput2 = [1; 3; 2]
let exampleInput3 = [2; 1; 3]
let exampleInput4 = [1; 2; 3]
let exampleInput5 = [2; 3; 1]
let exampleInput6 = [3; 2; 1]
let exampleInput7 = [3; 1; 2]


let initMaps l =
    let rec inner (n: Map<int, int>) (o: Map<int, int>) list i =
        match list with
        | [] -> (n, o)
        | (hd::tl) -> 
                    let newMap = n.Add(hd, i)
                    inner newMap n tl (i+1)
    inner Map.empty Map.empty l 1


let rec sayNumbers (newM: Map<int, int>) (prevM: Map<int, int>) i last target =
    if i = target then last
    else if not (prevM.ContainsKey last) 
    then
        let newM2 = newM.Add(0, i)
        sayNumbers newM2 newM (i+1) 0 target
    else
        let l = i - (prevM.[last] + 1)
        let newM2 = newM.Add(l, i)
        sayNumbers newM2 newM (i+1) l target

let solveA input =
    let (m1, m2) = initMaps input
    let turn = input.Length + 1
    let last = input.[input.Length - 1]
    sayNumbers m1 m2 turn last 2021

let exampleA1 =
    solveA exampleInput1

let exampleA2 =
    solveA exampleInput2

let exampleA3 =
    solveA exampleInput3

let exampleA4 =
    solveA exampleInput4

let exampleA5 =
    solveA exampleInput5

let exampleA6 =
    solveA exampleInput6

let exampleA7 =
    solveA exampleInput7

let a =
    solveA [2;0;1;9;5;19]

let solveB input =
    let (m1, m2) = initMaps input
    let turn = input.Length + 1
    let last = input.[input.Length - 1]
    sayNumbers m1 m2 turn last 30000001

// let exampleB1 =
//     solveB exampleInput1

let b =
    solveB [2;0;1;9;5;19]