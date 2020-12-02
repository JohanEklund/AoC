module Computer
#load "./computerTypes.fsx"
open ComputerTypes
#load "./computerListUtils.fsx"
open ComputerListUtils

let paramMode x =
    x |> function
            | 0 -> Positional
            | 1 -> Immediate

let firstParamMode ins =
    ins / 1000
    |> paramMode

let secondParamMode ins =
    (ins / 100) % 10
    |> paramMode

let getTwoParamModes ins =
    ((firstParamMode ins), (secondParamMode ins))

let parseAdd runtime =
    let ins = runtime.program.[runtime.pointer]
    let (firstMode, secondMode) = getTwoParamModes ins
    Add{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode};
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode};
        output = runtime.program.[runtime.pointer+3]
    }

let parseMultiply runtime =
    let ins = runtime.program.[runtime.pointer]
    let (firstMode, secondMode) = getTwoParamModes ins
    Multiply{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode};
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode};
        output = runtime.program.[runtime.pointer+3]
    }

let parseInput1 runtime =
    let output = runtime.program.[runtime.pointer+1]
    Input1{ input=runtime.input; output=output }

let parseOutput1 runtime =
    let paramMode = secondParamMode runtime.program.[runtime.pointer]
    let output = runtime.program.[runtime.pointer+1]
    Output1{ output={Value=output; Mode=paramMode}}

let parseJumpTrue runtime =
    let ins = runtime.program.[runtime.pointer]
    let (firstMode, secondMode) = getTwoParamModes ins
    JumpTrue{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode };
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode };
    }

let parseJumpFalse runtime =
    let ins = runtime.program.[runtime.pointer]
    let (firstMode, secondMode) = getTwoParamModes ins
    JumpFalse{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode };
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode };
    }

let parseLessThan runtime =
    let ins = runtime.program.[runtime.pointer]
    let (firstMode, secondMode) = getTwoParamModes ins
    LessThan{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode};
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode};
        output = runtime.program.[runtime.pointer+3]
    }

let parseEquals runtime =
    let ins = runtime.program.[runtime.pointer]
    let (firstMode, secondMode) = getTwoParamModes ins
    Equals{
        p1 = { Value=runtime.program.[runtime.pointer+1]; Mode=secondMode};
        p2 = { Value=runtime.program.[runtime.pointer+2]; Mode=firstMode};
        output = runtime.program.[runtime.pointer+3]
    }

let getCurrentInstruction runtime =
    runtime.program.[runtime.pointer]

let parseOperation runtime =
    let ins = getCurrentInstruction runtime
    ins % 100
    |> function
        | 1 -> parseAdd runtime
        | 2 -> parseMultiply runtime
        | 3 -> parseInput1 runtime
        | 4 -> parseOutput1 runtime
        | 5 -> parseJumpTrue runtime
        | 6 -> parseJumpFalse runtime
        | 7 -> parseLessThan runtime
        | 8 -> parseEquals runtime

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
        | Positional -> program.[param.Value]
        | Immediate -> param.Value
    
let replaceProgram runtime newProgram =
    { program = newProgram; pointer=runtime.pointer; input=runtime.input }

let add x y =
    x + y

let multiply x y =
    x * y

let equal x y =
    match x = y with
        | true -> 1
        | false -> 0

let lessThan x y =
    match x < y with
        | true -> 1
        | false -> 0

let doTwoParamMathOperation (op: TwoParameterOneOutput) runtime mathOp =
    let p1 = paramValue op.p1 runtime.program
    let p2 = paramValue op.p2 runtime.program
    replaceValueAt runtime.program op.output  (mathOp p1 p2)
    |> replaceProgram runtime

let doAdd op runtime =
    doTwoParamMathOperation op runtime add

let doMultiply op runtime =
    doTwoParamMathOperation op runtime multiply

let doLessThan runtime op =
    doTwoParamMathOperation op runtime lessThan

let doEquals runtime op =
    doTwoParamMathOperation op runtime equal

let Op3 runtime (op : Input1) =
    replaceValueAt runtime.program op.output op.input
    |> replaceProgram runtime

let Op4 runtime op =
    let v = paramValue op.output runtime.program
    printfn "Output:\t%i" v
    runtime

let doJumpTrue runtime op =
    paramValue op.p1 runtime.program
    |> function
        | 0 -> runtime
        | _ -> { program = runtime.program; pointer = (paramValue op.p2 runtime.program); input=runtime.input }
    
let doJumpFalse runtime op =
    paramValue op.p1 runtime.program
    |> function
        | 0 -> { program = runtime.program; pointer = (paramValue op.p2 runtime.program); input=runtime.input }
        | _ -> runtime

let handlePointer newPointer oldPointer op =
    match newPointer = oldPointer with
    | true -> incrementPointer op oldPointer
    | false -> newPointer

let handleInstruction runtime =
    let op = parseOperation runtime
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
