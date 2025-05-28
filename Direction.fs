module Direction

open Raylib_cs
open ProjectUtils
open Types

let reverse = function
    |Up     -> Down
    |Down   -> Up
    |Left   -> Right
    |Right  -> Left

let toVector = function
    |Up     ->  0, -1
    |Down   ->  0,  1
    |Right  ->  1,  0
    |Left   ->  -1,  0

let getUserDir memory =
    match Raylib.GetKeyPressed(), memory with
    |W, Down    -> Down
    |A, Right   -> Right
    |S, Up      -> Up
    |D, Left    -> Left
    |W, _       -> Up
    |A, _       -> Left
    |S, _       -> Down
    |D, _       -> Right
    |_ -> memory