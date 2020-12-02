type Password = { a: int; b: int; c: int; d: int; e: int; f: int}


let startNum =
    { a = 1; b = 9; c = 7; d = 4; e = 8; f = 7 }

let endNum =
    { a = 6; b = 7; c = 3; d = 2; e = 5; f = 1 }

let incrementA password =
    { a = password.a + 1; b = password.b; c = password.c; d = password.d; e = password.e; f = password.f; }

let incrementB password =
    let newPassword = { a = password.a; b = password.b + 1; c = password.c; d = password.d; e = password.e; f = password.f; }
    match newPassword.b with
    | 10 -> incrementA { a = password.a; b = 0; c = password.c; d = password.d; e = password.e; f = password.f; }
    | _ -> newPassword

let incrementC password =
    let newPassword = { a = password.a; b = password.b; c = password.c + 1; d = password.d; e = password.e; f = password.f; }
    match newPassword.c with
    | 10 -> incrementB { a = password.a; b = password.b; c = 0; d = password.d; e = password.e; f = password.f; }
    | _ -> newPassword

let incrementD password =
    let newPassword = { a = password.a; b = password.b; c = password.c; d = password.d + 1; e = password.e; f = password.f; }
    match newPassword.d with
    | 10 -> incrementC { a = password.a; b = password.b; c = password.c; d = 0; e = password.e; f = password.f; }
    | _ -> newPassword

let incrementE password =
    let newPassword = { a = password.a; b = password.b; c = password.c; d = password.d; e = password.e + 1; f = password.f; }
    match newPassword.e with
    | 10 -> incrementD { a = password.a; b = password.b; c = password.c; d = password.d; e = 0; f = password.f; }
    | _ -> newPassword

let incrementF password =
    let newPassword = { a = password.a; b = password.b; c = password.c; d = password.d; e = password.e; f = password.f + 1; }
    match newPassword.f with
    | 10 -> incrementE { a = password.a; b = password.b; c = password.c; d = password.d; e = password.e; f = 0; }
    | _ -> newPassword

let printPassword password =
    printfn "%i%i%i%i%i%i" password.a password.b password.c password.d password.e password.f

let twoAdjecentEqual password =
    password.a = password.b || password.b = password.c || password.c = password.d || password.d = password.e || password.e = password.f

let increaseOrSame password =
    password.a <= password.b && password.b <= password.c && password.c <= password.d && password.d <= password.e && password.e <= password.f

let isValid password =
    twoAdjecentEqual password && increaseOrSame password

let rec countValid password count validFunc =
    let newCount = if validFunc password then count + 1 else count
    let newPassword = incrementF password
    // if count <> newCount then printPassword password
    if newPassword = endNum then newCount
    else countValid newPassword newCount validFunc


let double1 password =
    password.a = password.b && password.a <> password.c

let double2 password =
    password.b = password.c && password.c <> password.d && password.a <> password.b

let double3 password =
    password.c = password.d && password.d <> password.e && password.b <> password.c

let double4 password =
    password.d = password.e && password.d <> password.c && password.e <> password.f

let double5 password =
    password.e = password.f && password.e <> password.d

let exactDouble password =
    double1 password || double2 password || double3 password || double4 password || double5 password

let isValid2 password = 
    twoAdjecentEqual password && increaseOrSame password && exactDouble password

let test0 = {a=1; b=1; c=2; d=2; e=3; f=3;}
let test1 = {a=1; b=2; c=3; d=4; e=4; f=4;}
let test2 = {a=1; b=1; c=1; d=1; e=2; f=2;}

let upgA =
    countValid startNum 0 isValid

let upgB =
    countValid startNum 0 isValid2