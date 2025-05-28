module Snake

#nowarn 3391

open Raylib_cs
open Types
open GlobalConsts
open ProjectUtils

let draw = function
    |Active (Snake snek, _, Food (foodX, foodY)) -> do
        Seq.iter
            (fun (x, y) ->
                Raylib.DrawRectangle
                    ( x * cellSize
                    , y * cellSize
                    , cellSize
                    , cellSize
                    , Color.Black ))
            snek

        Raylib.DrawRectangle
            ( foodX * cellSize
            , foodY * cellSize
            , cellSize
            , cellSize
            , Color.Blue )

    |GameOver (Score s) -> do
        let centreScreen = res >>> 1
        Raylib.DrawText
            ( $"GAME OVER!\nScore: {s}\nPlay again: SPACE"
            , 0
            , 0
            , 50
            , Color.Black)

let update = function
    |Active 
        ( Snake ( DeconstructLast (
            (headX, headY) :: body, tail))
        , rememberedDirection
        , Food food) ->

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
                    Snake
                        [ yield! nextPos :: (headX, headY) :: body
                        ; yield tail ]

                Active
                    ( newSnek
                    , Direction.getUserDir rememberedDirection
                    , Food.spawnNewFood newSnek )
                    
            |false ->
                Active 
                    ( nextPos :: (headX, headY) :: body |> Snake
                    , Direction.getUserDir rememberedDirection
                    , Food food )

            |true ->
                (GameOver << Score << (+) -1 << Seq.length) body

    |GameOver score ->
        if (Raylib.IsKeyDown KeyboardKey.Space:bool) then
            GameState.init()
        else
            GameOver score

    |_ -> failwith ""