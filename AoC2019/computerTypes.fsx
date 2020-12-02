module ComputerTypes

type ParamMode = Positional | Immediate

type Parameter = { Value: int; Mode: ParamMode }

type TwoParameterOneOutput = { p1: Parameter; p2: Parameter; output: int }

type TwoParameters = { p1: Parameter; p2: Parameter }

type Input1 = { input: int; output: int }

type Output1 = { output: Parameter }

type Operation =
    | Add of TwoParameterOneOutput
    | Multiply of TwoParameterOneOutput
    | Input1 of Input1
    | Output1 of Output1
    | JumpTrue of TwoParameters
    | JumpFalse of TwoParameters
    | LessThan of TwoParameterOneOutput
    | Equals of TwoParameterOneOutput

type Runtime = { program: int list; pointer: int; input: int }