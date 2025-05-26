module ProjectUtils

open Raylib_cs

//S' combinator aka phoenix aka fork
let phoenix binary unaryLeft unaryRight input =
    binary (unaryLeft input) (unaryRight input)

let (|W|A|S|D|Other|) i =
    if i = int KeyboardKey.W then W
    elif i = int KeyboardKey.A then A
    elif i = int KeyboardKey.S then S
    elif i = int KeyboardKey.D then D
    else Other

let (|DeconstructLast|_|) l = 
    l 
    |> List.rev 
    |> phoenix 
        (function 
            |Some x -> fun xs -> Some (List.rev xs, x)
            |None -> fun _ -> None) 
        List.tryHead 
        List.tail