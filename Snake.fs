module Snake

open Raylib_cs
open Types
open GlobalConsts

let init (x, y) =
    [x, y; x+1, y; x+2, y;x+3, y; x+4, y]

let draw snek =
    Seq.iter
        (fun (x, y) ->
            Raylib.DrawRectangle(
                x * cellSize, 
                y * cellSize, 
                cellSize, 
                cellSize, 
                Color.Black))
        snek

(*let update gamestate =
    let rec aux previousDir acc remaining =
        match remaining with
        |[] -> List.rev acc
            
        |(dir, x, y) :: tail ->
            let x', y' = Direction.toVector previousDir
            aux 
                dir 
                ((previousDir, x + x', y + y') :: acc)
                tail*)

let update gamestate =
    match gamestate.Snek with
    |(x, y) :: tail -> 
        let deltaX, deltaY = Direction.toVector gamestate.RememberedDirection
        let nextPos = (x + deltaX, y + deltaY)

        nextPos
        |> fun (x', y') ->
            List.fold 
                (fun hasCollided (a, b) -> hasCollided || (a = x') && (b = y'))
                false
                tail
            |> function false -> Some nextPos | true -> None
    |[] -> None

    |> function 
        |Some validNextPos -> 
            {   Snek = (validNextPos :: gamestate.Snek) |> ProjectUtils.cullLast
                RememberedDirection = Direction.getUserDir gamestate.RememberedDirection }

        |None ->
            gamestate