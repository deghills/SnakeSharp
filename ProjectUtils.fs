module ProjectUtils

open Raylib_cs

let (|W|A|S|D|Other|) i =
    if i = int KeyboardKey.W then W
    elif i = int KeyboardKey.A then A
    elif i = int KeyboardKey.S then S
    elif i = int KeyboardKey.D then D
    else Other