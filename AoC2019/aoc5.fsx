#load "./computerTypes.fsx"
#load "./computer.fsx"
#load "./AoCUtil.fsx"

open Computer
open ComputerTypes
open AoCUtil

let UpgA =
    let inputFile = "Day5\input.txt"
    let input = readCommaSeparatedInt inputFile
    let runtime = { program = input; pointer = 0; input = 1 }
    printfn "BEGIN A"
    handleProgram runtime |> ignore
    
// let testProgram = [3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99]
// let testRuntime = { program = testProgram; pointer = 0; input = 9}
// let testResult = handleProgram testRuntime |> ignore

let UpgB =
    let inputFile = "Day5\input.txt"
    let input = readCommaSeparatedInt inputFile
    let runtime = { program = input; pointer = 0; input = 5}
    printfn "BEGIN B"
    handleProgram runtime |> ignore
