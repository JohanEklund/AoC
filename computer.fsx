module Computer
#load "./computerTypes.fsx"
open ComputerTypes


let performAddOrMultiply ins x y =
    ins % 100
    |> function
        | 1 -> x + y
        | 2 -> x * y

let firstParamMode ins =
    ins / 1000
    |> function
        | 0 -> ParamMode.Positional
        | 1 -> ParamMode.Immediate

let secondParamMode ins =
    (ins / 100) % 10
    |> function
        | 0 -> ParamMode.Positional
        | 1 -> ParamMode.Immediate

let getTwoParamOperation runtime =
    let ins = runtime.program.[runtime.pointer]
    let firstMode = firstParamMode ins
    let secondMode = secondParamMode ins
    AddOrMultiply{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode};
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode};
        output = runtime.program.[runtime.pointer+3]
    }

let getInput1 runtime =
    let output = runtime.program.[runtime.pointer+1]
    Input1{ input=runtime.input; output=output }

let getOutput1 runtime =
    let paramMode = secondParamMode runtime.program.[runtime.pointer]
    let output = runtime.program.[runtime.pointer+1]
    Output1{ output={Value=output; Mode=paramMode}}

let getCurrentInstruction runtime =
    runtime.program.[runtime.pointer]

let getOperation runtime =
    let ins = getCurrentInstruction runtime
    ins % 100
    |> function
        | 1 | 2 -> getTwoParamOperation runtime
        | 3 -> getInput1 runtime
        | 4 -> getOutput1 runtime

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

let handleInstruction runtime =
    let ins = getCurrentInstruction runtime
    let op = getOperation runtime
    let newProgram = match op with
                        | AddOrMultiply (x) -> doAddOrMultiply ins x runtime.program
                        | Input1 (x) -> Op3 runtime.program x
                        | Output1 (x) -> Op4 runtime.program x
    let newPointer = incrementPointer op runtime.pointer
    {program=newProgram; pointer=newPointer; input=runtime.input}

let rec handleProgram runtime =
    let a = getCurrentInstruction runtime
    if a = 99 then runtime.program
    else
        let newRuntime = handleInstruction runtime
        handleProgram newRuntime


