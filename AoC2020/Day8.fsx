#load "./utils.fsx"
#load "./computerListUtils.fsx"
open ComputerListUtils
open AoCUtil

type Operation = Acc | Jmp | Nop

type Instruction = { Op: Operation; Arg: int }

let testInput = [
    "nop +0";
    "acc +1";
    "jmp +4";
    "acc +3";
    "jmp -3";
    "acc -99";
    "acc +1";
    "jmp -4";
    "acc +6"
]

let strToInstr (str: string) =
    let s = str.Split(" ");
    let a = int s.[1]
    match s.[0] with
    | "nop" -> { Op = Nop; Arg = a; }
    | "acc" -> { Op = Acc; Arg = a; }
    | "jmp" -> { Op = Jmp; Arg = a; }
    | _ -> 
            printfn "shit happened %s" s.[0]
            { Op = Jmp; Arg = System.Int32.MaxValue; }


let execInstr instr ptr acc =
    match instr.Op with
    | Nop -> (ptr + 1, acc)
    | Jmp -> (ptr + instr.Arg, acc)
    | Acc -> (ptr + 1, acc + instr.Arg)


let rec runProgram (prg: Instruction list) ptr acc (executed: int list) =
    if List.contains ptr executed then (acc, ptr)
    else if ptr = prg.Length then (acc, ptr)
    else        
        let (newPtr, newAcc) = execInstr prg.[ptr] ptr acc
        runProgram prg newPtr newAcc (executed @[ptr])


let example1 =
    let prg = testInput |> List.map(strToInstr)
    let (acc, _) = runProgram prg 0 0 []
    acc

let a = 
    let prg = readLines "./AoC2020/input8.txt" |> List.map(strToInstr)
    let (acc, _) = runProgram prg 0 0 []
    acc

let replaceProgram prg instr ptr =
    replaceValueAt prg ptr instr

let rec runAndFix (origPrg: Instruction list) newPrg ptr =
    let (acc, endedAt) = runProgram newPrg 0 0 []
    if endedAt = origPrg.Length then acc
    else
        match origPrg.[ptr].Op with
        | Nop -> runAndFix origPrg (replaceProgram origPrg { Op=Jmp; Arg = origPrg.[ptr].Arg } ptr) (ptr + 1)
        | Jmp -> runAndFix origPrg (replaceProgram origPrg { Op=Nop; Arg = origPrg.[ptr].Arg } ptr) (ptr + 1)
        | Acc -> runAndFix origPrg newPrg (ptr + 1)

let example2 =
    let prg = testInput |> List.map(strToInstr)
    let ptr = runAndFix prg prg 0
    ptr

let b =
    let prg = readLines "./AoC2020/input8.txt" |> List.map(strToInstr)
    let ptr = runAndFix prg prg 0
    ptr