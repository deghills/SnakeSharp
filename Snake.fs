module Snake

open Raylib_cs
open Types
open GlobalConsts
open ProjectUtils

let init (x, y) =
    [x, y; x+1, y; x+2, y;x+3, y; x+4, y; x+5, y; x+6, y; x+7, y; x+8, y; x+9, y; x+10, y; x+11, y]

let draw (snek, _) =
    Seq.iter
        (fun (x, y) ->
            Raylib.DrawRectangle(
                x * cellSize, 
                y * cellSize, 
                cellSize, 
                cellSize, 
                Color.Black))
        snek

let update = function
    |(DeconstructLast ((x, y) :: body, snekLast), rememberedDirection) -> 
        let deltaX, deltaY = Direction.toVector rememberedDirection
        let nextPos = (x + deltaX, y + deltaY)

        nextPos
        |> fun (x', y') 
            -> body
            |> List.fold 
                (fun hasCollided (a, b) -> hasCollided || (a = x' && b = y'))
                false
            |> function 
                | false -> nextPos :: (x, y) :: body, Direction.getUserDir rememberedDirection 
                | true -> [ yield! (x, y) :: body; yield snekLast ], rememberedDirection
    |_ -> failwith "wahappen??"