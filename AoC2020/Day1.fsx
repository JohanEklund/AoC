#load "./utils.fsx"

open AoCUtil

let inputAsc = readIntLines "./AoC2020/input1.txt"
                |> List.sortBy (id)

let sum2020 x y =
    let sum = x + y
    match sum with
        | (z) when z = 2020 -> Some(x, y)
        | _ -> None

let sum2020ThreeInputs x y z =
    let sum = x + y + z
    match sum with
        | (a) when a = 2020 -> Some(x, y, z)
        | _ -> None

let find1 x y =    
    x |> List.map (fun e -> sum2020 e y)

let find2 x y =
    y |> List.map (fun e -> find1 x e)

let find11 x y z =
    x |> List.map (fun e -> sum2020ThreeInputs e y z)

let find22 x y z =
    y |> List.map (fun e -> find11 x e z)

let find33 x y z =
    z |> List.map (fun e -> find22 x y e)  

let a = find2 inputAsc inputAsc
        |> List.collect (id)
        |> List.sortByDescending (id)
        |> List.choose (id)
        |> List.head
        |> fun(x, y) -> x * y

let b = find33 inputAsc inputAsc inputAsc
        |> List.collect (id)
        |> List.collect (id)
        |> List.sortByDescending (id)
        |> List.choose (id)
        |> List.head
        |> fun(x, y, z) -> x * y * z
        