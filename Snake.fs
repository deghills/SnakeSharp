module Snake

open Raylib_cs
open Types
open GlobalConsts
open ProjectUtils

let init (x, y) =
    [ x     , y
    ; x + 1 , y
    ; x + 2 , y]

let draw (snek, _, Food (foodX, foodY)) = do
    Seq.iter
        (fun (x, y) ->
            Raylib.DrawRectangle
                ( x * cellSize
                , y * cellSize
                , cellSize
                , cellSize
                , Color.Black))
        snek

    Raylib.DrawRectangle
        ( foodX * cellSize
        , foodY * cellSize
        , cellSize
        , cellSize
        , Color.Blue )

let update = function
    |DeconstructLast ((headX, headY) :: body, tail), rememberedDirection, Food food ->
        let deltaX, deltaY = 
            Direction.toVector rememberedDirection
        let nextPos 
            = headX + deltaX
            , headY + deltaY

        (headX, headY) :: body
        |> List.contains nextPos
        |> (||) 
            (let x', y' = nextPos
            gridSize-1 < x' || x' < 0 ||
            gridSize-1 < y' || y' < 0)
        |> function 
            |false when nextPos = food ->
                let newSnek = 
                    [ yield! nextPos :: (headX, headY) :: body
                    ; yield tail ]

                newSnek
                , Direction.getUserDir rememberedDirection
                , Food.spawnNewFood newSnek
                    
            |false ->
                nextPos :: (headX, headY) :: body
                , Direction.getUserDir rememberedDirection
                , Food food

            |true -> 
                [ yield! (headX, headY) :: body
                ; yield tail ]
                , rememberedDirection
                , Food food

    |_ -> failwith ""