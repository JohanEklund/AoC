module Day1

open AoCUtil

let test = [
    "1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"
]

let test2 = [
    "two1nine"
    "eightwothree"
    "abcone2threexyz"
    "xtwone3four"
    "4nineeightseven2"
    "zoneight234"
    "7pqrstsixteen"
]

let getNumbers str =
    let numbers = set ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';]
    str |> Seq.where (fun x -> numbers.Contains x )

let rec getNumbersRec (str: string) acc =    
    if str.Length = 0 then acc
    else if (str.StartsWith("1")) then getNumbersRec (str.Remove(0, 1)) $"{acc}1"
    else if (str.StartsWith("2")) then getNumbersRec (str.Remove(0, 1)) $"{acc}2"
    else if (str.StartsWith("3")) then getNumbersRec (str.Remove(0, 1)) $"{acc}3"
    else if (str.StartsWith("4")) then getNumbersRec (str.Remove(0, 1)) $"{acc}4"
    else if (str.StartsWith("5")) then getNumbersRec (str.Remove(0, 1)) $"{acc}5"
    else if (str.StartsWith("6")) then getNumbersRec (str.Remove(0, 1)) $"{acc}6"
    else if (str.StartsWith("7")) then getNumbersRec (str.Remove(0, 1)) $"{acc}7"
    else if (str.StartsWith("8")) then getNumbersRec (str.Remove(0, 1)) $"{acc}8"
    else if (str.StartsWith("9")) then getNumbersRec (str.Remove(0, 1)) $"{acc}9"
    else if (str.StartsWith("one")) then (getNumbersRec (str.Remove(0, 1))) $"{acc}1"
    else if (str.StartsWith("two")) then (getNumbersRec (str.Remove(0, 1))) $"{acc}2"
    else if (str.StartsWith("three")) then (getNumbersRec (str.Remove(0, 1))) $"{acc}3"
    else if (str.StartsWith("four")) then (getNumbersRec (str.Remove(0, 1))) $"{acc}4"
    else if (str.StartsWith("five")) then (getNumbersRec (str.Remove(0, 1))) $"{acc}5"
    else if (str.StartsWith("six")) then (getNumbersRec (str.Remove(0, 1))) $"{acc}6"
    else if (str.StartsWith("seven")) then (getNumbersRec (str.Remove(0, 1))) $"{acc}7"
    else if (str.StartsWith("eight")) then (getNumbersRec (str.Remove(0, 1))) $"{acc}8"
    else if (str.StartsWith("nine")) then (getNumbersRec (str.Remove(0, 1))) $"{acc}9"
    else getNumbersRec (str.Remove(0, 1)) acc

let rec getLastInList ls =
    match ls with
    | [] -> failwith "cannot get last in empty list"
    | (hd::tl) ->
        if tl.Length >= 1
        then getLastInList tl
        else hd

let numberPair candidate =
    let length = candidate |> Seq.length
    match length with
    | 2 -> candidate
    | i when i < 2 -> [candidate.Head; candidate.Head]
    | i when i > 2 -> [candidate.Head; getLastInList candidate]
    | _ -> failwith ""

let day1A = 
    readLines @".\input.txt"
    |> Seq.map (fun x -> getNumbers x)    
    |> Seq.map (fun x -> numberPair (x |> Seq.toList))
    |> Seq.map (fun x -> $"{x.[0]}{x.[1]}")
    |> Seq.map (fun x -> x |> int)
    |> Seq.sum

let day1B = 
    readLines @".\input.txt"
    // test2
    |> Seq.map (fun x -> getNumbersRec x "")
    |> Seq.map (fun x -> numberPair (x.ToCharArray() |> Array.toList))
    |> Seq.map (fun x -> $"{x.[0]}{x.[1]}")
    |> Seq.map (fun x -> x |> int)
    |> Seq.sum