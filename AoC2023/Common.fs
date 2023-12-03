module AoCUtil

let readLines filePath = 
    let x = System.IO.File.ReadLines(filePath)
    List.ofSeq x

let readIntLines filePath = 
    let strLines = readLines filePath
    List.map int strLines

let readCommaSeparatedInt filePath = 
    let line = System.IO.File.ReadAllText(filePath)
    line.Split ','
    |> Seq.toList
    |> List.map int

let readCommaSeparatedStr filePath =
    System.IO.File.ReadLines filePath
    |> Seq.map (fun e -> e.Split ',')
    |> Seq.map (fun e -> Seq.toList e)
    |> Seq.toList

let printAll arr =
    arr |> List.iter (fun f -> printfn "%O" f)
    id

let printAllSeq s =
    s |> Seq.toList |> printAll