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
    |DeconstructLast ((x, y) :: body, tail), rememberedDirection, Food food ->
        let deltaX, deltaY = Direction.toVector rememberedDirection
        let nextPos = (x + deltaX, y + deltaY)

        (x, y) :: body
        |> List.contains nextPos
        |> (||) 
            (let x', y' = nextPos
            0 > x' || x' > gridSize-1 ||
            0 > y' || y' > gridSize-1)
        |> function 
            | false when nextPos = food ->
                let newSnek = [ yield! nextPos :: (x, y) :: body; yield tail ]
                newSnek
                ,Direction.getUserDir rememberedDirection
                ,Food.spawnNewFood newSnek
                    
            | false ->
                nextPos :: (x, y) :: body
                ,Direction.getUserDir rememberedDirection
                ,Food food

            | true -> 
                [ yield! (x, y) :: body; yield tail ]
                ,rememberedDirection
                ,Food food

    |_ -> failwith "wahappen??"