#load "./AoCUtil.fsx"

open AoCUtil

let UpdatedValue newValue oldValue currentIndex wantedIndex =
    match currentIndex = wantedIndex with
    | true -> newValue
    | false -> oldValue

let replaceValueAt list index newValue = 
    list
    |> Seq.mapi (fun i elem -> UpdatedValue newValue elem i index)
    |> Seq.toList

let zeroOrValue i1 i2 value =
    match i1 = i2 with
    | true -> value
    | false -> 0

let getValueAt list index = 
    list
    |> Seq.mapi (fun i elem -> zeroOrValue index i elem)
    |> Seq.sum

let getReferencedValue list index = 
    getValueAt list index
    |> getValueAt list

let handleAddOperation program index1 index2 resultPos =
    let value1 = getReferencedValue program index1
    let value2 = getReferencedValue program index2
    let r = getValueAt program resultPos
    replaceValueAt program r (value1 + value2)

let handleMultiplicationOperation program index1 index2 resultPos =
    let value1 = getReferencedValue program index1
    let value2 = getReferencedValue program index2
    let r = getValueAt program resultPos
    replaceValueAt program r (value1 * value2)

let readOperation program position =
    getValueAt program position
    |> function
        | 1 -> handleAddOperation program (position + 1) (position + 2) (position + 3)
        | 2 -> handleMultiplicationOperation program (position + 1) (position + 2) (position + 3)
        | _ -> []
    
let rec runProgram program position =
    let newProgram = readOperation program position

    if List.isEmpty newProgram then program
    else runProgram newProgram (position + 4)

let runProgramB program noun verb = 
    let a = replaceValueAt program 1 noun
    let b = replaceValueAt a 2 verb
    let c = runProgram b 0
    getValueAt c 0
    
let UpgA =
    let inputFile = "./AoC2019/Day2/input.txt"
    let input = readCommaSeparatedInt inputFile
    runProgramB input 12 2 // 4138687


let rec findNounVerb program noun verb goal = 
    let result = runProgramB program noun verb
    if result = goal then (noun, verb)
    elif noun < 99 then findNounVerb program (noun + 1) verb goal
    else findNounVerb program 0 (verb + 1) goal
    


let UpgB =
    let inputFile = "./AoC2019/Day2/input.txt"
    let input = readCommaSeparatedInt inputFile
    let noun, verb = findNounVerb input 0 0 19690720
    100 * noun + verb // 6635
