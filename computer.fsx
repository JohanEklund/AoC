module Computer

// type Operations = Add=1 | Mult=2 | SingIn=3 | SingOut=4 | Invalid=666

type ParamMode = Positional=0 | Immediate=1
type Parameter = { Value: int; Mode: ParamMode }

type AddOrMultiply = { p1: Parameter; p2: Parameter; output: int }
type Input1 = { input: int; output: int }
type Output1 = { output: Parameter }

type Operation =
    | AddOrMultiply of AddOrMultiply
    | Input1 of Input1
    | Output1 of Output1

type Runtime = { program: int list; pointer: int}


let add x y =
    x + y

let multiply x y =
    x * y

let performAddOrMultiply ins x y =
    ins % 100
    |> function
        | 1 -> add x y
        | 2 -> multiply x y
        | _ -> 666

let firstParamMode ins =
    ins / 1000
    |> function
        | 0 -> ParamMode.Positional
        | _ -> ParamMode.Immediate

let secondParamMode ins =
    (ins / 100) % 10
    |> function
        | 0 -> ParamMode.Positional
        | _ -> ParamMode.Immediate

let getTwoParamOperation pointer (program: int list) =
    let ins = program.[pointer]
    let firstMode = firstParamMode ins
    let secondMode = secondParamMode ins
    AddOrMultiply{
        p1 = { Value=program.[pointer+1]; Mode=secondMode};
        p2 = { Value=program.[pointer+2]; Mode=firstMode};
        output = program.[pointer+3]
    }

let getInput1 pointer (program: int list) input =
    let output = program.[pointer+1]
    Input1{ input=input; output=output }

let getOutput1 pointer (program: int list) =
    let paramMode = secondParamMode program.[pointer]
    let output = program.[pointer+1]
    Output1{ output={Value=output; Mode=paramMode}}

let getCurrentInstruction runtime =
    runtime.program.[runtime.pointer]

let getOperation runtime input =
    let ins = getCurrentInstruction runtime
    ins % 100
    |> function
        | 1 | 2 -> getTwoParamOperation runtime.pointer runtime.program
        | 3 -> getInput1 runtime.pointer runtime.program input
        | 4 -> getOutput1 runtime.pointer runtime.program 

let incrementPointer ins curr =
    match ins with    
    | AddOrMultiply (_) -> curr + 4
    | Input1 (_) -> curr + 2
    | Output1 (_) -> curr + 2

let paramValue param (program: int list) =
    param.Mode
    |> function
        | ParamMode.Positional -> program.[param.Value]
        | ParamMode.Immediate -> param.Value

let UpdatedValue newValue oldValue currentIndex wantedIndex =
    match currentIndex = wantedIndex with
    | true -> newValue
    | false -> oldValue

let replaceValueAt list index newValue = 
    list
    |> Seq.mapi (fun i elem -> UpdatedValue newValue elem i index)
    |> Seq.toList
    
let doAddOrMultiply ins (p: AddOrMultiply) program =
    let p1 = paramValue p.p1 program
    let p2 = paramValue p.p2 program
    let res = performAddOrMultiply ins p1 p2
    replaceValueAt program p.output res

let Op3 program (op : Input1) =
    replaceValueAt program op.output op.input

let Op4 program (op : Output1) =
    let v = paramValue op.output program
    printfn "Output:\t%i" v
    program

let handleInstruction runtime input =
    let ins = getCurrentInstruction runtime
    let op = getOperation runtime input
    let newProgram = match op with
                        | AddOrMultiply (x) -> doAddOrMultiply ins x runtime.program
                        | Input1 (x) -> Op3 runtime.program x
                        | Output1 (x) -> Op4 runtime.program x
    let newPointer = incrementPointer op runtime.pointer
    {program=newProgram; pointer=newPointer}

let rec handleProgram runtime input =
    let a = getCurrentInstruction runtime
    if a = 99 then runtime.program
    else
        let newRuntime = handleInstruction runtime input
        handleProgram newRuntime input


