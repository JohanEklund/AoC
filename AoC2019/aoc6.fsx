#load "./AoCUtil.fsx"

open AoCUtil

type Orbit = { Parent: string; Children: Orbit list}

let parseOrbits (str : string) =
    let split = str.Split ')'
    { Parent = split.[0]; Children = [{Parent = split.[1]; Children = []}]}


let findChildren allOrbits parent =
    allOrbits
    |> List.filter (fun x -> x.Parent = parent)

let merge a b =
    a @ b
    |> Seq.distinct
    |> List.ofSeq


let adsf orbits children =
    let a = children
            |> List.map (fun x -> findChildren orbits x.Parent ) 
    let first = a.[0]
    a
    |> List.map (fun x -> first @ x)
    |> Seq.distinct
    |> List.ofSeq

let connectParentWithChild orbit allOrbits =
    let children = adsf allOrbits orbit
    { Parent = orbit.Parent; Children = children}

let UpgA = 
    let inputFile = "inputs\d6.txt"
    let data = readLines inputFile |> List.map parseOrbits
    let root = { Parent = "COM"; Children = [{ Parent = "Q1H"; Children = []}]}
    let asdf = connectParentWithChild root data
