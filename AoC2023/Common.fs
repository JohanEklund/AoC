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


let Eratosthenes max =
    let primes = [|for i in 0..max -> true|]
    primes.SetValue(false, 0)
    primes.SetValue(false, 1)
    let setMultiplesToFalse arr m =
        arr |> Array.iteri (fun i v -> 
            if v && m <> i && i % m = 0 then arr.SetValue(false, i)
        )
    let loop =
        let mutable i = 0
        while i*i < max do 
            if primes.[i] then setMultiplesToFalse primes i
            i <- i+1
    loop
    primes

let primeFactors number (primes: bool array) =
    let rec primeFactorsRec number i (acc: int list) =
        if i*i > number then number::acc
        else if not primes.[i] then primeFactorsRec number (i+1) acc
        else if number % i = 0 then primeFactorsRec (number/i) i (i::acc)
        else primeFactorsRec number (i+1) acc
    primeFactorsRec number 2 []

let inlinePrint title list  =
    printfn "%s %A" title list
    list

let MGM numbers =
    let p = Eratosthenes (numbers |> List.max)
    let max l =
        l
            |> List.sortByDescending(fun (a, b) -> b)
            |> List.take 1
    let rec spread acc x y =
        if y = 1 then x::acc
        else spread (x::acc) x (y-1) 
    numbers
        |> List.map (fun i -> primeFactors i p)        
        |> List.map (fun f -> f |> List.countBy id)
        |> List.collect (fun f -> f)
        |> List.groupBy (fun (x, _) -> x)
        |> List.map (fun (_, l) -> max l )
        |> List.collect (fun f -> f |> List.map(fun (a,b) -> spread [] a b))
        |> List.collect id        
        |> List.map int64
        |> List.fold(fun a v -> a*v) 1L

