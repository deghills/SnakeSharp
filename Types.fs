module Types

open Raylib_cs
open GlobalConsts

type Direction =
        Up | Down | Left | Right

type Snake = Snake of (int*int) list with
    static member init (x, y) = 
        Snake
            [ x,        y 
            ; x + 1,    y
            ; x + 2,    y ]

type Food = Food of (int*int) with
    static member spawnNewFood =
        let rec aux (Snake snek) =
            let gridSizeDecr = gridSize - 1
            let food = Raylib.GetRandomValue(0, gridSizeDecr), Raylib.GetRandomValue(0, gridSizeDecr)

            snek
            |> List.contains food
            |> function true -> aux (Snake snek) | false -> Food food

        aux

type Score = Score of int

type GameState = 
    | ActiveGame of Snake*Direction*Food
    | GameOver of Score with

    static member init() =
        let centre = gridSize >>> 1
        let snek = Snake.init (centre, centre)
        ActiveGame
            ( snek
            , Left
            , Food.spawnNewFood snek )