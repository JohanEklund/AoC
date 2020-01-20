module ComputerTypes

type ParamMode = Positional=0 | Immediate=1

type Parameter = { Value: int; Mode: ParamMode }

type Add = { p1: Parameter; p2: Parameter; output: int }

type Multiply = { p1: Parameter; p2: Parameter; output: int }

type Input1 = { input: int; output: int }

type Output1 = { output: Parameter }

type JumpTrue = { p1: Parameter; p2: Parameter }

type JumpFalse = { p1: Parameter; p2: Parameter }

type LessThan = { p1: Parameter; p2: Parameter; output: int }

type Equals = { p1: Parameter; p2: Parameter; output: int }

type Operation =
    | Add of Add
    | Multiply of Multiply
    | Input1 of Input1
    | Output1 of Output1
    | JumpTrue of JumpTrue
    | JumpFalse of JumpFalse
    | LessThan of LessThan
    | Equals of Equals

type Runtime = { program: int list; pointer: int; input: int }