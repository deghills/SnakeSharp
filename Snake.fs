module Snake

open Raylib_cs
open Types
open GlobalConsts
open ProjectUtils

let init (x, y) =
    [x, y; x+1, y]

let draw (snek, _, Food (foodX, foodY)) = do
    Seq.iter
        (fun (x, y) ->
            Raylib.DrawRectangle(
                x * cellSize, 
                y * cellSize, 
                cellSize, 
                cellSize, 
                Color.Black))
        snek

    Raylib.DrawRectangle(
        foodX * cellSize,
        foodY * cellSize,
        cellSize,
        cellSize,
        Color.Green)

let update = function
    |DeconstructLast ((x, y) :: body, snekLast), rememberedDirection, Food (foodX, foodY) ->
        let deltaX, deltaY = Direction.toVector rememberedDirection
        let nextPos = (x + deltaX, y + deltaY)

        nextPos
        |> fun (x', y') 
            -> body
            |> List.fold 
                (fun hasCollided (a, b) -> hasCollided || (a = x' && b = y'))
                false
            |> function 
                | false when x' = foodX && y' = foodY ->
                    let newSnek = [ yield! nextPos :: (x, y) :: body; yield snekLast ]
                    newSnek
                    ,Direction.getUserDir rememberedDirection
                    ,Food.spawnNewFood newSnek
                    
                | false ->
                    nextPos :: (x, y) :: body
                    ,Direction.getUserDir rememberedDirection
                    ,Food (foodX, foodY)

                | true -> 
                    [ yield! (x, y) :: body; yield snekLast ]
                    ,rememberedDirection
                    ,Food (foodX, foodY)
    |_ -> failwith "wahappen??"