module Snake

#nowarn 3391

open Raylib_cs
open Types
open GlobalConsts
open ProjectUtils

let draw = function
    |ActiveGame (Snake snek, _, Food (foodX, foodY)) -> do
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
        Raylib.DrawText
            ( $"GAME OVER!\nScore: {s}\nPlay again: SPACE"
            , 0
            , 0
            , 50
            , Color.Black )

let update = function
    |ActiveGame
        (Snake (DeconstructLast (
            head :: body, tail))
        , rememberedDirection
        , Food food) ->

        let nextPos = 
            (fun (a, b) (c, d) -> a + c, b + d)
                head 
                (Direction.toVector rememberedDirection)
            |> taurus

        (head :: body)
        |> List.contains nextPos
        |> function 
            |false when nextPos = food ->
                let newSnek = 
                    Snake
                        [ yield! nextPos :: head :: body
                        ; yield tail ]

                ActiveGame
                    ( newSnek
                    , Direction.getUserDir rememberedDirection
                    , Food.spawnNewFood newSnek )
                    
            |false ->
                ActiveGame 
                    ( nextPos :: head :: body |> Snake
                    , Direction.getUserDir rememberedDirection
                    , Food food )

            |true ->
                (GameOver << Score << (+) -1 << Seq.length) body

    |GameOver score ->
        if (Raylib.IsKeyDown KeyboardKey.Space:bool) then
            GameState.init()
        else
            GameOver score

    |_ -> GameOver (Score -1)