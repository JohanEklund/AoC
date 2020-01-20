module Computer
#load "./computerTypes.fsx"
open ComputerTypes
#load "./computerListUtils.fsx"
open ComputerListUtils

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

let getAdd runtime =
    let ins = runtime.program.[runtime.pointer]
    let firstMode = firstParamMode ins
    let secondMode = secondParamMode ins
    Add{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode};
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode};
        output = runtime.program.[runtime.pointer+3]
    }

let getMultiply runtime =
    let ins = runtime.program.[runtime.pointer]
    let firstMode = firstParamMode ins
    let secondMode = secondParamMode ins
    Multiply{
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

let getJumpTrue runtime =
    let ins = runtime.program.[runtime.pointer]
    let firstMode = firstParamMode ins
    let secondMode = secondParamMode ins
    JumpTrue{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode };
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode };
    }

let getJumpFalse runtime =
    let ins = runtime.program.[runtime.pointer]
    let firstMode = firstParamMode ins
    let secondMode = secondParamMode ins
    JumpFalse{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode };
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode };
    }

let getLessThan runtime =
    let ins = runtime.program.[runtime.pointer]
    let firstMode = firstParamMode ins
    let secondMode = secondParamMode ins
    LessThan{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode};
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode};
        output = runtime.program.[runtime.pointer+3]
    }

let getEquals runtime =
    let ins = runtime.program.[runtime.pointer]
    let firstMode = firstParamMode ins
    let secondMode = secondParamMode ins
    Equals{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode};
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode};
        output = runtime.program.[runtime.pointer+3]
    }

let getCurrentInstruction runtime =
    runtime.program.[runtime.pointer]

let getOperation runtime =
    let ins = getCurrentInstruction runtime
    ins % 100
    |> function
        | 1 -> getAdd runtime
        | 2 -> getMultiply runtime
        | 3 -> getInput1 runtime
        | 4 -> getOutput1 runtime
        | 5 -> getJumpTrue runtime
        | 6 -> getJumpFalse runtime
        | 7 -> getLessThan runtime
        | 8 -> getEquals runtime

let incrementPointer ins curr =
    match ins with    
    | Add (_) -> curr + 4
    | Multiply (_) -> curr + 4
    | Input1 (_) -> curr + 2
    | Output1 (_) -> curr + 2
    | JumpTrue (_) -> curr + 3
    | JumpFalse (_) -> curr + 3
    | LessThan (_) -> curr + 4
    | Equals (_) -> curr + 4

let paramValue param (program: int list) =
    param.Mode
    |> function
        | ParamMode.Positional -> program.[param.Value]
        | ParamMode.Immediate -> param.Value
    
let replaceProgram runtime newProgram =
    { program = newProgram; pointer=runtime.pointer; input=runtime.input }

let doAdd (op: Add) runtime =
    let p1 = paramValue op.p1 runtime.program
    let p2 = paramValue op.p2 runtime.program
    replaceValueAt runtime.program op.output (p1 + p2)
    |> replaceProgram runtime

let doMultiply (op: Multiply) runtime =
    let p1 = paramValue op.p1 runtime.program
    let p2 = paramValue op.p2 runtime.program
    replaceValueAt runtime.program op.output (p1 * p2)
    |> replaceProgram runtime

let Op3 runtime (op : Input1) =
    replaceValueAt runtime.program op.output op.input
    |> replaceProgram runtime

let Op4 runtime (op : Output1) =
    let v = paramValue op.output runtime.program
    printfn "Output:\t%i" v
    runtime

let doJumpTrue runtime (op: JumpTrue) =
    paramValue op.p1 runtime.program
    |> function
        | 0 -> runtime
        | _ -> { program = runtime.program; pointer = (paramValue op.p2 runtime.program); input=runtime.input }
    
let doJumpFalse runtime (op: JumpFalse) =
    paramValue op.p1 runtime.program
    |> function
        | 0 -> { program = runtime.program; pointer = (paramValue op.p2 runtime.program); input=runtime.input }
        | _ -> runtime

let doLessThan runtime (op: LessThan) =
    let p1 = paramValue op.p1 runtime.program
    let p2 = paramValue op.p2 runtime.program
    let value = match p1 < p2 with
                    | true -> 1
                    | false -> 0
    replaceValueAt runtime.program op.output value
    |> replaceProgram runtime                

let doEquals runtime (op: Equals) =
    let p1 = paramValue op.p1 runtime.program
    let p2 = paramValue op.p2 runtime.program
    let value = match p1 = p2 with
                    | true -> 1
                    | false -> 0
    replaceValueAt runtime.program op.output value
    |> replaceProgram runtime                

let handlePointer newPointer oldPointer op =
    match newPointer = oldPointer with
    | true -> incrementPointer op oldPointer
    | false -> newPointer
    

let handleInstruction runtime =
    let ins = getCurrentInstruction runtime
    let op = getOperation runtime
    let newRuntime = match op with
                        | Add (x) -> doAdd x runtime
                        | Multiply (x) -> doMultiply x runtime
                        | Input1 (x) -> Op3 runtime x
                        | Output1 (x) -> Op4 runtime x
                        | JumpTrue (x) -> doJumpTrue runtime x
                        | JumpFalse (x) -> doJumpFalse runtime x
                        | LessThan (x) -> doLessThan runtime x
                        | Equals (x) -> doEquals runtime x
    let newPointer = handlePointer newRuntime.pointer runtime.pointer op
    {program=newRuntime.program; pointer=newPointer; input=runtime.input}

let rec handleProgram runtime =
    let a = getCurrentInstruction runtime
    if a = 99 then runtime.program
    else
        let newRuntime = handleInstruction runtime
        handleProgram newRuntime


