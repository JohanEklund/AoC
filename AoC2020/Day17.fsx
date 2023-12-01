


let exampleInput = [
    ".#.";
    "..#";
    "###"
    ]

let initZ size =
    let z = [ for i in 1 .. size -> 
                ([ for i in 1 .. size -> '.'])
            ]
    z

let shouldBeActive x y z d =
    let getNeighbours x y z d =
        let xs = d.[z].[y].[x+1]


let exampleA =
    let z0 = initZ exampleInput.Length
    let z1 = exampleInput |> List.map(fun e -> e.ToCharArray())
    let z2 = initZ exampleInput.Length
    (z0, z1, z2)
    