module ComputerTypes

type ParamMode = Positional=0 | Immediate=1

type Parameter = { Value: int; Mode: ParamMode }

type AddOrMultiply = { p1: Parameter; p2: Parameter; output: int }

type Input1 = { input: int; output: int }

type Output1 = { output: Parameter }

type Operation =
    | AddOrMultiply of AddOrMultiply
    | Input1 of Input1
    | Output1 of Output1

type Runtime = { program: int list; pointer: int; input: int }