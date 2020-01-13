#load "./computer.fsx"
#load "./AoCUtil.fsx"

open Computer
open AoCUtil

let UpgA =
    let inputFile = "./AoC2019/Day5/input.txt"
    let input = readCommaSeparatedInt inputFile
    let runtime = { program = input; pointer = 0 }
    printfn "BEGIN"
    handleProgram runtime 1 |> ignore



let UpgB =
    let inputFile = "./AoC2019/Day5/input.txt"
    let input = readCommaSeparatedInt inputFile
    printfn "BEGIN B"