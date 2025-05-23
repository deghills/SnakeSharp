module Snake

open Raylib_cs
open Types
open GlobalConsts

let init (x, y) =
    [Left, x, y; Left, x+1, y; Left, x+2, y; Left, x+3, y; Left, x+4, y]

let draw snek =
    Seq.iter
        (fun (_, x, y) ->
            Raylib.DrawRectangle(
                x * cellSize, 
                y * cellSize, 
                cellSize, 
                cellSize, 
                Color.Black))
        snek

let update gamestate =
    let rec aux previousDir acc remaining =
        match remaining with
        |[] -> List.rev acc
            
        |(dir, x, y) :: tail ->
            let x', y' = Direction.toVector previousDir
            aux 
                dir 
                ((previousDir, x + x', y + y') :: acc)
                tail

    let newDir = Direction.getUserDir gamestate.RememberedDirection
                
    {   Snek = 
            aux 
                newDir
                [] 
                gamestate.Snek 
            
        RememberedDirection = 
            newDir }